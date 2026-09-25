(ns glimmer-tui.curses
  "The ncurses implementation of glimmer-tui.screen, plus terminal setup and
  teardown and the raw input read.

  Everything here runs on one thread — the event loop's. ncurses is not
  thread-safe, which costs nothing in practice because glimmer.backend already
  requires that off-thread work be posted to the UI thread through :schedule.

  Colour is indexed, and only indexed. Pairs are allocated lazily as
  (foreground, background) combinations turn up and cached, because init_pair
  costs a terminfo round trip and a UI reuses a handful of combinations for its
  whole life. Colour -1 means \"the terminal's own default\", which is what keeps
  a glimmer UI transparent over the user's theme rather than painting it black.

  How many indices are available is whatever terminfo says (`tigetnum`), and
  glimmer-tui.color folds anything richer — a hex string, an r/g/b triple, a
  256-palette index on a 16-colour tty — down to something this terminal has.
  Nothing binds the ncurses 6.1 direct-colour entry points, so a hex prop costs
  no portability: it is resolved to a palette index before init_pair sees it."
  (:require [glimmer-tui.color :as color]
            [glimmer-tui.ffi :as c]
            [jolt.ffi :as ffi]))

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
  "The colour pair number for the palette indices fg/bg, allocating one on first
  use. Pair 0 is reserved by ncurses for the default colours, so allocation
  starts at 1. A terminal has a finite number of pairs (COLOR_PAIRS, 64 on an
  eight-colour tty); past that the request falls back to the default pair rather
  than failing, because a UI that loses a colour is better than one that dies."
  [state fg bg]
  (if (and (neg? fg) (neg? bg))
    0
    (let [k [fg bg]]
      (or (get-in @state [:pairs k])
          (let [n (:next-pair @state)]
            (if (>= n (:max-pairs @state))
              0
              (do (c/init-pair n fg bg)
                  (swap! state #(-> % (assoc-in [:pairs k] n) (assoc :next-pair (inc n))))
                  n)))))))

(defn ncurses-screen
  "A glimmer-tui.screen backed by stdscr. `win` is the pointer initscr returned."
  [win]
  (let [colors (if (c/has-colors) (max 0 (c/tigetnum "colors")) 0)
        state (atom {:pairs {} :next-pair 1
                     :colors? (pos? colors)
                     :profile (color/profile colors)
                     ;; terminfo reports pairs too; 0 or ERR means "assume the
                     ;; classic 64" rather than refusing to allocate any.
                     :max-pairs (let [p (c/tigetnum "pairs")] (if (pos? p) p 64))})]
    {:size (fn [] [(c/getmaxx win) (c/getmaxy win)])
     :clear! (fn [] (c/werase win) nil)
     :put!
     (fn [x y s style]
       ;; ncurses clips writes to the window, but a negative origin is a
       ;; programming error rather than a clip, so guard it here.
       (when (and (>= y 0) (>= x 0) (< y (c/getmaxy win)) (< x (c/getmaxx win)))
         (let [profile (:profile @state)
               pair (if (:colors? @state)
                      (pair-for! state
                                 (color/resolve-color (:fg style) profile)
                                 (color/resolve-color (:bg style) profile))
                      0)]
           (c/wattrset win (bit-or (attrs-of style) (c/color-pair pair)))
           ;; -1 means "the whole string, clipped at the right margin", which
           ;; sidesteps having to count UTF-8 bytes for the length argument.
           ;; ncursesw decodes the bytes itself once the ctype locale is set.
           (c/mvwaddnstr win y x s -1)
           (c/wattrset win c/A-NORMAL)))
       nil)
     :cursor! (fn [x y visible?]
                ;; start! sets leaveok, which lets ncurses skip the move to the
                ;; window's cursor position on refresh. Clear it before placing a
                ;; caret, or the wmove never reaches the physical cursor, and put
                ;; it back when hiding so a caret-less UI keeps the saving.
                (if visible?
                  (do (c/leaveok win 0) (c/wmove win y x) (c/curs-set 1))
                  (do (c/curs-set 0) (c/leaveok win 1)))
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
    ;; "dumb" has a terminfo entry, so setupterm would accept it, but it has no
    ;; cursor addressing and cannot run a UI.
    (boolean (and t (not= "" t) (not= "dumb" t)))))

(defn- terminfo-ok?
  "Whether ncurses can actually load a terminfo entry for $TERM. Checking the
  name is not enough: a runner may export TERM=xterm-256color while shipping
  none of the terminfo database, and that is an exit(), not an error return."
  []
  (let [errret (ffi/alloc 4)]
    (try
      (ffi/write errret :int 0 0)
      (zero? (c/setupterm ffi/null 1 errret))
      (catch :default _ false)
      (finally (ffi/free errret)))))

(defn usable?
  "Whether start! can take over the terminal: stdout is a tty, TERM names
  something usable, and ncurses can load its terminfo entry."
  []
  (and (tty?) (usable-term?) (terminfo-ok?)))

;; --- bracketed paste and mouse reporting --------------------------------------
;; In bracketed-paste mode a terminal wraps pasted text in CSI 200~ ... CSI 201~,
;; which is the only way to tell a pasted newline from a pressed Return: without
;; it a pasted stack trace arrives as keystrokes and every line break activates
;; the focused field. glimmer-tui.core decodes the wrapper into a :paste event;
;; this end of it is terminal lifecycle, so it belongs with start! and stop!.
;;
;; Mouse is the same story. ncurses' own mouse decoding is not used at all:
;; its ABI differs by build (the button shift is 6 bits under mouse version 1,
;; which is what stock macOS ships, and 5 under version 2), and a version-1 build
;; has no button 5, so a wheel-down is simply not representable. Turning on SGR
;; reporting and decoding ESC [ < b ; x ; y M|m here sidesteps both: the report
;; is version-independent and carries wheel-up (64) and wheel-down (65) alike.
;; glimmer-tui.keys/sgr-mouse does the decoding.
;;
;; None of these private modes has a terminfo capability or an ncurses entry
;; point, so they are written to fd 1 by hand. A terminal that has never heard of
;; one ignores a set it does not know, which is why this is safe unconditionally.
;;
;; 1000 (button press/release) and 1006 (SGR encoding) are all this needs. 1002,
;; which adds motion while a button is held, is left off on purpose: there is no
;; drag gesture to map that motion onto — a press is a click — so it would only
;; arrive as a click on every cell the pointer crossed.
(def ^:private paste-mode-on "\u001b[?2004h")
(def ^:private paste-mode-off "\u001b[?2004l")
(def ^:private mouse-mode-on "\u001b[?1000h\u001b[?1006h")   ; button events, SGR encoding
(def ^:private mouse-mode-off "\u001b[?1006l\u001b[?1000l")

(defn- emit!
  "Write `s` straight to standard output, around ncurses rather than through it."
  [s]
  (c/write-fd 1 s (count s))
  nil)

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
  (when-not (terminfo-ok?)
    (throw (ex-info (str "glimmer-tui: no terminfo entry for TERM="
                         (System/getenv "TERM") ", cannot start a UI")
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
    (c/keypad win 1)               ; decode arrows, F-keys, KEY_RESIZE
    (c/notimeout win 0)            ; use the escape-sequence timer for ESC
    (c/curs-set 0)
    (c/leaveok win 1)
    (when (c/has-colors)
      (c/start-color)
      (c/use-default-colors))
    ;; Mouse is NOT set up through ncurses: mousemask would make ncurses parse
    ;; incoming reports itself and hand back KEY_MOUSE, which is the version-1
    ;; path this backend exists to avoid. The reporting modes are turned on below
    ;; and the raw reports decoded in glimmer-tui.core instead.
    (emit! paste-mode-on)
    (emit! mouse-mode-on)
    (c/flushinp)
    win))

(defn stop!
  "Hand the terminal back. Safe to call twice, which matters because the event
  loop's finally clause and an error handler may both reach for it."
  []
  (when-not (c/isendwin)
    (c/curs-set 1)
    (c/endwin)
    ;; after endwin, so they land on a terminal ncurses has already flushed and
    ;; handed back rather than in the middle of its teardown
    (emit! mouse-mode-off)
    (emit! paste-mode-off))
  nil)

(defn read-key
  "Wait up to `timeout-ms` for a key and return its code, or nil on timeout.
  A short timeout is what lets the event loop also drain work posted from other
  threads."
  [win timeout-ms]
  (c/wtimeout win timeout-ms)
  (let [ch (c/wgetch win)]
    (when (not= ch c/ERR) ch)))
