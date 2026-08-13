(ns glimmer-tui.curses
  "The ncurses implementation of glimmer-tui.screen, plus terminal setup and
  teardown and the raw input read.

  Everything here runs on one thread — the event loop's. ncurses is not
  thread-safe, which costs nothing in practice because glimmer.backend already
  requires that off-thread work be posted to the UI thread through :schedule.

  Colour is 8-colour indexed on purpose. Pairs are allocated lazily as
  (foreground, background) combinations turn up and cached, because init_pair
  costs a terminfo round trip and a UI reuses a handful of combinations for its
  whole life. Colour -1 means \"the terminal's own default\", which is what keeps
  a glimmer UI transparent over the user's theme rather than painting it black."
  (:require [glimmer-tui.ffi :as c]
            [jolt.ffi :as ffi]))

(def ^:private color-index
  {:black c/COLOR-BLACK :red c/COLOR-RED :green c/COLOR-GREEN
   :yellow c/COLOR-YELLOW :blue c/COLOR-BLUE :magenta c/COLOR-MAGENTA
   :cyan c/COLOR-CYAN :white c/COLOR-WHITE :default c/COLOR-DEFAULT})

(defn- ->color [k] (get color-index k c/COLOR-DEFAULT))

(defn- attrs-of
  "The ncurses attribute word for a style map (colour pair excluded)."
  [style]
  (cond-> 0
    (:bold style)      (bit-or c/A-BOLD)
    (:dim style)       (bit-or c/A-DIM)
    (:underline style) (bit-or c/A-UNDERLINE)
    (:reverse style)   (bit-or c/A-REVERSE)
    (:blink style)     (bit-or c/A-BLINK)))

(defn- pair-for!
  "The colour pair number for fg/bg, allocating one on first use. Pair 0 is
  reserved by ncurses for the default colours, so allocation starts at 1."
  [state fg bg]
  (if (and (= fg :default) (= bg :default))
    0
    (let [k [fg bg]]
      (or (get-in @state [:pairs k])
          (let [n (:next-pair @state)]
            (c/init-pair n (->color fg) (->color bg))
            (swap! state #(-> % (assoc-in [:pairs k] n) (assoc :next-pair (inc n))))
            n)))))

(defn ncurses-screen
  "A glimmer-tui.screen backed by stdscr. `win` is the pointer initscr returned."
  [win]
  (let [state (atom {:pairs {} :next-pair 1 :colors? (pos? (c/has-colors))})]
    {:size (fn [] [(c/getmaxx win) (c/getmaxy win)])
     :clear! (fn [] (c/werase win) nil)
     :put!
     (fn [x y s style]
       ;; ncurses clips writes to the window, but a negative origin is a
       ;; programming error rather than a clip, so guard it here.
       (when (and (>= y 0) (>= x 0) (< y (c/getmaxy win)) (< x (c/getmaxx win)))
         (let [pair (if (:colors? @state)
                      (pair-for! state (or (:fg style) :default) (or (:bg style) :default))
                      0)]
           (c/wattrset win (bit-or (attrs-of style) (c/color-pair pair)))
           ;; -1 means "the whole string, clipped at the right margin", which
           ;; sidesteps having to count UTF-8 bytes for the length argument.
           ;; ncursesw decodes the bytes itself once the ctype locale is set.
           (c/mvwaddnstr win y x s -1)
           (c/wattrset win c/A-NORMAL)))
       nil)
     :cursor! (fn [x y visible?]
                (if visible?
                  (do (c/wmove win y x) (c/curs-set 1))
                  (c/curs-set 0))
                nil)
     :present! (fn [] (c/wnoutrefresh win) (c/doupdate) nil)}))

;; --- terminal lifecycle ------------------------------------------------------
;; initscr does not report failure, it calls exit(). In a REPL that loses the
;; session; in CI it kills the job with no output at all. So the two conditions
;; that make it fail are checked up front and reported as ordinary exceptions.
;;
;; A pty with no usable TERM is the normal state of a CI runner: `script` hands
;; you a pseudo-terminal, terminfo then has nothing to look up, and ncurses
;; exits. Checking both is what keeps `jolt smoke` honest about being skipped
;; rather than mysteriously dead.
(defn- tty? [] (pos? (c/isatty 1)))

(defn- usable-term? []
  (let [t (System/getenv "TERM")]
    (boolean (and t (not= "" t) (not= "dumb" t)))))

(defn usable?
  "Whether start! can take over the terminal: stdout is a tty and TERM names
  something terminfo can plausibly look up."
  []
  (and (tty?) (usable-term?)))

(defn start!
  "Put the terminal into raw, keypad, mouse-reporting mode and return the stdscr
  pointer. Locale is set first: without a UTF-8 ctype, ncursesw renders every
  multibyte glyph as a question mark."
  []
  (when-not (tty?)
    (throw (ex-info "glimmer-tui: stdout is not a terminal, cannot start a UI"
                    {:fd 1})))
  (when-not (usable-term?)
    (throw (ex-info (str "glimmer-tui: TERM is "
                         (if-let [t (System/getenv "TERM")] (pr-str t) "unset")
                         ", cannot start a UI")
                    {:term (System/getenv "TERM")})))
  ;; category 0 is LC_ALL on macOS and LC_CTYPE on glibc; 6 is LC_ALL on glibc
  ;; and an unknown category (harmless NULL) on macOS. Between them, the ctype
  ;; locale is set from the environment on both platforms.
  (c/setlocale 0 "")
  (c/setlocale 6 "")
  (let [win (c/initscr)]
    (c/raw)                        ; keys as typed, and ctrl-c as a key (see ffi)
    (c/noecho)                     ; the UI decides what appears, not the tty
    (c/nonl)                       ; keep Return distinguishable from newline
    (c/keypad win 1)               ; decode arrows, F-keys, KEY_MOUSE, KEY_RESIZE
    (c/notimeout win 0)            ; use the escape-sequence timer for ESC
    (c/curs-set 0)
    (c/leaveok win 1)
    (when (pos? (c/has-colors))
      (c/start-color)
      (c/use-default-colors))
    (c/mousemask c/ALL-MOUSE-EVENTS ffi/null)
    ;; 0 disables click resolution: press and release arrive separately and
    ;; immediately, which keeps the UI responsive instead of waiting to see
    ;; whether a double-click is coming.
    (c/mouseinterval 0)
    (c/flushinp)
    win))

(defn stop!
  "Hand the terminal back. Safe to call twice, which matters because the event
  loop's finally clause and an error handler may both reach for it."
  []
  (when (zero? (c/isendwin))
    (c/curs-set 1)
    (c/endwin))
  nil)

(defn read-key
  "Wait up to `timeout-ms` for a key and return its code, or nil on timeout.
  A short timeout is what lets the event loop also drain work posted from other
  threads."
  [win timeout-ms]
  (c/wtimeout win timeout-ms)
  (let [ch (c/wgetch win)]
    (when (not= ch c/ERR) ch)))

(defn read-mouse
  "Pull the pending mouse event as {:x :y :bstate}, or nil. Called after wgetch
  returns KEY_MOUSE."
  []
  (let [buf (ffi/alloc c/MEVENT-SIZE)]
    (try
      (when (zero? (c/getmouse buf))
        {:x (ffi/read buf :int c/MEVENT-X-OFFSET)
         :y (ffi/read buf :int c/MEVENT-Y-OFFSET)
         :bstate (ffi/read buf :ulong c/MEVENT-BSTATE-OFFSET)})
      (finally (ffi/free buf)))))
