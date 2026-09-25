(ns glimmer-tui.core
  "The terminal backend for glimmer. Requiring this namespace installs it, after
  which glimmer's portable reconciler renders hiccup into a curses screen:

    (ns myapp
      (:require [glimmer.ratom :refer [atom]]
                [glimmer.core :as ui]
                [glimmer-tui.core]))          ; installs the ncurses backend

    (defn -main [& _] (ui/run my-app))

  What this namespace adds to the widget layer is the part a GUI toolkit would
  normally own: an event loop, keyboard focus, hit testing, scrolling and timers.

  Focus lives here rather than in glimmer's core because it is a property of how
  a toolkit is driven, not of the component model — GTK gets focus from GTK. The
  focus ring is recomputed from the widget tree on every repaint, in tree order,
  so a component that renders a new button gets a sensible tab position with no
  registration step. Focus follows the widget id, so a keyed list can reorder
  around the focused row without the focus jumping to a different item, and when
  focus lands inside a scroll container the container scrolls to show it.

  A key is offered to the focused widget first, then to each of its ancestors on
  the way out — which is how Page Down reaches the scroll container a focused
  button happens to be sitting in — and only then to the loop's own bindings
  (Tab, Enter, quit). Mouse button 1 focuses and activates whatever is under the
  pointer; the wheel scrolls whatever scroll container is under it.

  A paste is not typing and is not delivered as typing: the terminal is put into
  bracketed-paste mode, and the wrapper it sends is decoded here into a single
  {:type :paste :text \"...\"} event, dispatched down the same path. That is what
  keeps a pasted newline from pressing Return halfway through a stack trace."
  (:require [clojure.string :as str]
            [glimmer.backend :as b]
            [glimmer-tui.curses :as curses]
            [glimmer-tui.ffi :as c]
            [glimmer-tui.keys :as keys]
            [glimmer-tui.layout :as layout]
            [glimmer-tui.render :as render]
            [glimmer-tui.screen :as scr]
            [glimmer-tui.widget :as w]
            [glimmer-tui.widgets]
            [glimmer-tui.widgets.overlay :as overlay]
            [glimmer-tui.widgets.scroll :as scroll]))

;; The running app: {:root widget :screen s :win ptr :tree laid-out-snapshot
;;                   :focus-id id :quit? bool}. nil when nothing is running.
(defonce ^:private app (atom nil))

;; Work posted from other threads, run on the loop thread at the top of a tick.
(defonce ^:private pending (atom []))

(defn- schedule
  "glimmer.backend's :schedule. A ratom mutated on an nREPL worker thread must
  not repaint from that thread, so the re-render is queued here and the loop
  picks it up on its next tick — the terminal equivalent of g_idle_add."
  [work]
  (swap! pending conj work)
  nil)

(defn- drain!
  "Run everything queued by `schedule`. compare-and-set! rather than a plain
  reset!, so work posted while the queue is being taken is not dropped."
  []
  (loop []
    (let [q @pending]
      (when (seq q)
        (if (compare-and-set! pending q [])
          (doseq [f q] (f))
          (recur))))))

;; --- timers ------------------------------------------------------------------
;; A UI that shows a spinner, a clock or a progress bar needs something to happen
;; without a key being pressed. The loop already wakes up every :tick-ms, so a
;; timer is a due time and a thunk; `pump-timers!` fires whatever is ready. Both
;; are safe to call from another thread, and both run their thunk ON the loop
;; thread, which is the only thread allowed to touch widgets.
(defonce ^:private timers (atom {:next-id 0 :entries {}}))

(defn- now-ms [] (System/currentTimeMillis))

(defn- add-timer! [ms every? f]
  (let [id (:next-id (swap! timers update :next-id inc))]
    (swap! timers assoc-in [:entries id]
           {:due (+ (now-ms) ms) :every (when every? ms) :f f})
    id))

(defn after!
  "Run `f` on the loop thread in about `ms` milliseconds. Returns an id for
  `cancel!`. Resolution is the loop's :tick-ms, so a 1ms timer is a next-tick
  timer."
  [ms f] (add-timer! ms false f))

(defn every!
  "Run `f` on the loop thread about every `ms` milliseconds until cancelled.
  This is what drives a spinner or a clock:

    (every! 80 #(swap! tick inc))"
  [ms f] (add-timer! ms true f))

(defn cancel!
  "Stop the timer `id`."
  [id] (swap! timers update :entries dissoc id) nil)

(defn cancel-all!
  "Stop every timer. The loop does this when it exits, so a repeating timer does
  not outlive the UI it was animating."
  [] (swap! timers assoc :entries {}) nil)

(defn pump-timers!
  "Fire every timer that is due. The event loop calls this once a tick; a test
  drives it by hand."
  []
  (let [t (now-ms)
        due (filter (fn [[_ e]] (<= (:due e) t)) (:entries @timers))]
    (doseq [[id e] due]
      (if-let [period (:every e)]
        (swap! timers assoc-in [:entries id :due] (+ t period))
        (swap! timers update :entries dissoc id))
      ((:f e)))
    (boolean (seq due))))

;; --- painting ----------------------------------------------------------------
(defn- scroll-node? [n] (= :scroll (w/container-kind (:tag n))))

(defn- focus-scope
  "The subtree focus is confined to: the topmost modal overlay if one is on
  screen, otherwise the whole tree. A dialog that did not trap focus would let
  Tab wander into the form behind it, which is the classic terminal-UI bug."
  [tree]
  (or (last (filter overlay/modal? (w/walk tree))) tree))

(defn- bindings-for
  "The key bindings in effect for the focused widget: its own, plus those of the
  scroll containers it sits inside, which answer the keys it declines. This is
  what a :help widget renders when it is given no bindings of its own."
  [tree focus-id]
  (when-let [path (and focus-id (w/path-to tree focus-id))]
    (let [own (or (w/bindings-of (last path)) [])
          inherited (mapcat #(or (w/bindings-of %) [])
                            (filter scroll-node? (butlast path)))
          seen (set (map first own))]
      (into (vec own) (remove (fn [[action _]] (contains? seen action)) inherited)))))

(defn- ensure-focus-visible!
  "Scroll every scroll container between the root and the focused widget just
  enough to show it. Returns true when something moved, which means the tree has
  to be laid out again."
  [tree root focus-id]
  (boolean
    (when-let [path (and focus-id (w/path-to tree focus-id))]
      (let [target (last path)
            scrolls (filter scroll-node? (butlast path))]
        (when (:rect target)
          (reduce (fn [moved? s]
                    (if-let [wid (w/find-widget root (:id s))]
                      (or (scroll/ensure-visible! wid s (:rect target)) moved?)
                      moved?))
                  false
                  (reverse scrolls)))))))

(defn paint!
  "Snapshot, lay out and paint the tree rooted at widget `root` onto `screen`.
  Returns the laid-out snapshot. Public because it is the whole render pipeline
  in one call: tests drive it against an in-memory screen, with no terminal."
  ([root screen] (paint! root screen nil))
  ([root screen focus-id]
   (let [[cols rows] (scr/size screen)
         ctx {:focus-id focus-id :bindings (bindings-for (w/snapshot root) focus-id)}]
     (w/derive-state! root ctx)
     (let [tree (layout/layout (w/snapshot root) cols rows)]
       (render/render! screen tree ctx)
       tree))))

(defn attach!
  "Point the backend at widget `root`, painting onto `screen`, without touching a
  terminal. The event loop calls this; so do the tests, and so could any other
  driver — a scripted demo, or a session served over a socket. Everything below
  (frame!, press!, click!) works the same whether the screen is a real terminal
  or an in-memory buffer."
  [root screen]
  (reset! app {:root root :screen screen :win nil :focus-id nil :quit? false})
  nil)

(defn focus-id
  "The id of the focused widget, or nil."
  [] (:focus-id @app))

(defn focus!
  "Move focus to the widget with `id`, if it can take it."
  [id]
  (when (some #(= % id) (:ring @app))
    (swap! app assoc :focus-id id)
    (w/touch!)
    id))

(defn- resolve-focus
  "Keep the focus on a live widget: hold the current one while it is still in
  the ring, otherwise fall back to whatever asked for focus with :autofocus, and
  failing that to the first focusable widget (or nothing).

  :autofocus matters more in a terminal than it looks. Tree order puts the focus
  on whatever is highest on the screen, which is usually a filter field — and a
  focused text field swallows every letter, so an application's single-key
  bindings are dead until the user presses Tab. Saying which widget starts with
  the focus is how an app avoids that."
  [tree ring focus-id]
  (if (and focus-id (some #(= % focus-id) ring))
    focus-id
    (let [in-ring? (set ring)]
      (or (first (keep (fn [n] (when (and (:autofocus (:props n)) (in-ring? (:id n)))
                                 (:id n)))
                       (w/walk tree)))
          (first ring)))))

(defn- repaint! []
  (let [{:keys [root screen]} @app
        [cols rows] (scr/size screen)]
    ;; Laying out can move a scroll container (to bring the focused widget into
    ;; view), which changes the layout — so it is done to a fixed point, bounded
    ;; because each pass can only ever scroll towards the target.
    (loop [pass 0]
      (let [snap (w/snapshot root)
            ;; focus needs no geometry: it is tree order over what can take it
            ring (w/focus-ring (focus-scope snap))
            focus (resolve-focus snap ring (:focus-id @app))
            binds (bindings-for snap focus)
            ctx {:focus-id focus :bindings binds}]
        ;; widgets that depend on the loop rather than on props (a help bar)
        ;; get told before anything is measured, since it changes their size
        (w/derive-state! root ctx)
        (let [tree (layout/layout (w/snapshot root) cols rows)
              ;; Only a CHANGE of focus scrolls: otherwise scrolling a container
              ;; whose focused widget went off-screen would immediately scroll
              ;; back, and the wheel would do nothing at all.
              moved? (and (< pass 2)
                          (not= focus (:visible-focus @app))
                          (ensure-focus-visible! tree root focus))]
          (if moved?
            (recur (inc pass))
            (do (swap! app assoc :tree tree :ring ring :focus-id focus
                       :visible-focus focus)
                (render/render! screen tree ctx)
                nil)))))))

(defn frame!
  "Lay out and paint the current tree, refreshing the focus ring. Returns the
  laid-out snapshot."
  []
  (repaint!)
  (:tree @app))

(defn tree
  "The laid-out snapshot of the last painted frame."
  [] (:tree @app))

;; --- input -------------------------------------------------------------------
(defn- focused-widget []
  (let [{:keys [root focus-id]} @app]
    (when focus-id (w/find-widget root focus-id))))

(defn- move-focus! [delta]
  (let [{:keys [ring focus-id]} @app
        n (count ring)]
    (when (pos? n)
      (let [i (or (first (keep-indexed (fn [i id] (when (= id focus-id) i)) ring)) 0)
            next-i (mod (+ i delta) n)]
        (swap! app assoc :focus-id (nth ring next-i))
        (w/touch!))))
  nil)

(defn- activate! [widget]
  (when widget
    (when-let [f (:activate (w/spec-for (:tag @widget)))]
      (f widget)
      (w/touch!)))
  nil)

(defn usable-terminal?
  "Whether ui/run would be able to take over the terminal: stdout is a tty and
  TERM names something terminfo knows. Code that must not die on a headless
  machine — a smoke test, or an app with a batch mode — can check first."
  []
  (curses/usable?))

(defn quit!
  "Ask the event loop to stop. Safe from any thread and from a handler."
  []
  (swap! app assoc :quit? true)
  nil)

(defn bindings
  "The key bindings the focused widget and its scroll ancestors answer to, as
  [action spec] pairs. A :help widget renders these on its own; this is for an
  application that wants to render them somewhere else."
  []
  (bindings-for (:tree @app) (:focus-id @app)))

(defn click!
  "Focus and activate whatever focusable widget covers the cell (x, y). A widget
  with a :click handler (a list, a table) is told WHERE it was clicked so it can
  select the row under the pointer; everything else is simply activated. Returns
  the widget id that took the click, or nil when the cell is dead space."
  [x y]
  (let [{:keys [root tree]} @app]
    (when-let [n (w/hit tree x y)]
      (swap! app assoc :focus-id (:id n))
      (let [wid (w/find-widget root (:id n))
            spec (w/spec-for (:tag n))]
        (if-let [f (:click spec)]
          (do (f wid x y {:node n :tree tree}) (w/touch!))
          (activate! wid)))
      (:id n))))

(defn scroll!
  "Scroll whatever scroll container covers the cell (x, y) by `lines`. Returns
  true when something moved."
  [x y lines]
  (let [{:keys [root tree]} @app]
    (boolean
      (when-let [n (w/hit-kind tree :scroll x y)]
        (when-let [wid (w/find-widget root (:id n))]
          (scroll/scroll-by! wid n 0 lines))))))

(defn- close-overlay!
  "Ask the topmost modal overlay to close. Returns true when there was one."
  []
  (let [{:keys [root tree]} @app]
    (boolean
      (when-let [n (last (filter overlay/modal? (w/walk tree)))]
        (when-let [f (:on-close (:props n))]
          (f)
          (w/touch!)
          true)))))

(defn- handle-mouse!
  "Act on a decoded mouse event (see glimmer-tui.keys/sgr-mouse): a wheel scrolls
  whatever container is under the pointer — `wheel-lines` lines a notch, three by
  default — and a left press focuses and activates (or selects in) whatever is
  under it. Releases are ignored: acting on the press alone keeps a click
  immediate rather than waiting to see whether a double-click is coming."
  [{:keys [x y wheel button action]}]
  (let [lines (or (:wheel-lines (:props (:tree @app))) 3)]
    (cond
      (and (#{:up :down} wheel) (= action :press)) (scroll! x y (if (= wheel :up) (- lines) lines))
      (and (= 0 button) (= action :press)) (click! x y)))
  nil)

(defn- dispatch!
  "Offer `event` to the focused widget, then to each of its ancestors. The first
  one that says it handled the key stops the walk."
  [event]
  (let [{:keys [root tree focus-id]} @app
        path (when focus-id (w/path-to tree focus-id))]
    (boolean
      (some (fn [n]
              (when-let [f (:key (w/spec-for (:tag n)))]
                (when-let [wid (w/find-widget root (:id n))]
                  (when (f wid event {:node n :tree tree})
                    (w/touch!)
                    true))))
            (reverse path)))))

(defn press!
  "Feed a key to the UI, exactly as the event loop does. `key` is either an
  ncurses key code or an already-decoded event (see glimmer-tui.keys). Tab, Esc
  and the quit keys are the loop's; everything else is offered to the focused
  widget and its ancestors first. `quit-keys` defaults to none, which is what a
  test wants."
  ([key] (press! key #{}))
  ([key quit-keys]
   (let [event (if (map? key) key (keys/decode key))]
     (cond
       (= :resize (:type event)) (w/touch!)
       (= :mouse (:type event))  (handle-mouse! event)
       (= :tab (:type event))    (move-focus! 1)
       (= :back-tab (:type event)) (move-focus! -1)
       :else
       (when-not (dispatch! event)
         (cond
           ;; The quit keys are checked AFTER the widgets have passed, so that a
           ;; text field typing `q` types a q. Nothing in the widget set consumes
           ;; a ctrl chord it was not asked to, so ctrl-c always gets through.
           (keys/match? event quit-keys) (quit!)
           ;; Esc closes a dialog, likewise only once nothing else wanted it
           (= :escape (:type event)) (close-overlay!)
           (contains? #{:enter :space} (:type event)) (activate! (focused-widget))
           :else nil))))
   nil))

;; --- bracketed paste ---------------------------------------------------------
;; The terminal is in bracketed-paste mode (curses/start!), so a paste arrives
;; wrapped in ESC [ 200~ ... ESC [ 201~ and is turned into one
;; {:type :paste :text "..."} event here. Without that, pasted text is
;; indistinguishable from typing: every line break is a Return, so pasting a
;; stack trace into a one-line field sends it as a dozen messages with the line
;; breaks dropped.

;; Bytes of a paste arrive 1-7ms apart where a person's keystrokes are hundreds
;; (see issue #5), so a silence this long means the end marker is never coming —
;; a terminal that died mid-paste stalls the loop for this and no longer.
(def ^:private paste-timeout-ms 100)

;; Codes read while looking for a marker that turned out not to be one. They are
;; handed back in order, before anything new is read, so a sequence that only
;; looked like a paste is typed exactly as it arrived.
(defonce ^:private held-over (atom []))

(defn- take-held-over! []
  (when-let [c (first @held-over)]
    (swap! held-over subvec 1)
    c))

(defn- after-escape!
  "Read what follows an ESC for as long as it could still be a paste marker or an
  SGR mouse report. Returns [marker codes]: `marker` is :paste-start, :paste-end,
  :mouse or nil, and `codes` is everything read, so a run that was none of those
  can be given back.

  `timeout` is how long to wait for each code. Outside a paste it is 0 — ESC and
  the key after it arrive together, and waiting would turn a pressed Escape into
  half an alt chord. Inside one it is the paste timeout, because a paste big
  enough to span two reads can put the end marker's ESC at the boundary.

  `codes` seeds the run with bytes ncurses already consumed (see decode-input!)."
  ([read timeout] (after-escape! read timeout []))
  ([read timeout codes]
   (loop [codes codes]
     (let [marker (keys/escape-run codes)]
       (if (not= :partial marker)
         [marker codes]
         (if-let [c (read timeout)]
           (recur (conj codes c))
           [nil codes]))))))

(defn- paste-text
  "The codes collected between the markers, as text. A terminal sends CR for a
  line break inside a paste — the same byte Return sends — so they are turned
  back into newlines here, and a CRLF into one."
  [codes]
  (-> (apply str (map char codes))
      (str/replace "\r\n" "\n")
      (str/replace "\r" "\n")))

(defn- read-paste!
  "The text between the start marker and ESC [ 201~."
  [read]
  (loop [acc []]
    (let [code (read paste-timeout-ms)]
      (cond
        (nil? code) (paste-text acc)
        (= 27 code) (let [[marker codes] (after-escape! read paste-timeout-ms)]
                      (if (= :paste-end marker)
                        (paste-text acc)
                        ;; an ESC the paste itself carried: keep it and carry on
                        (recur (into (conj acc 27) codes))))
        :else (recur (conj acc code))))))

(defn decode-input!
  "One event from `read`, a function of a timeout in milliseconds that answers a
  key code or nil — the terminal, or a scripted one in a test.

  ESC followed immediately by another key is an alt (meta) chord rather than two
  keystrokes: that is how a terminal sends alt-b, and the only way to tell them
  apart is that nothing human types a key within a millisecond of Escape. The
  exceptions are the two runs that are also ESC and then bytes: a bracketed-paste
  marker, which becomes one :paste event carrying the whole paste, and an SGR
  mouse report, which becomes one :mouse event.

  A report can also start with KEY_MOUSE rather than ESC: where terminfo has
  kmous=\\E[< (ncurses 6's xterm entries), keypad mode matches those three bytes
  itself and leaves the rest of the report as plain characters. That is read as
  the report it is. KEY_MOUSE only arrives because reporting is on, so whatever
  follows it is the terminal's, not the user's, and a run that does not parse is
  dropped rather than typed."
  [read tick-ms]
  (when-let [code (or (take-held-over!) (read tick-ms))]
    (cond
      (= c/KEY-MOUSE code)
      (let [[marker codes] (after-escape! read 0 [91 60])]
        (when (= :mouse marker) (keys/sgr-mouse codes)))

      (= 27 code)
      (let [[marker codes] (after-escape! read 0)]
        (case marker
          :paste-start (keys/paste (read-paste! read))
          ;; an end marker with nothing to end: the paste is already over
          :paste-end nil
          :mouse (keys/sgr-mouse codes)
          (if-let [next-code (first codes)]
            (let [e (keys/decode next-code)]
              (swap! held-over into (rest codes))
              {:type :alt :ch (:ch e) :base-type (:type e) :code next-code})
            (keys/decode 27))))

      :else (keys/decode code))))

;; --- the event loop ----------------------------------------------------------
(defn- read-event!
  "One key from the terminal, decoded."
  [win tick-ms]
  (decode-input! (fn [timeout] (curses/read-key win timeout)) tick-ms))

(defn- run!
  "glimmer.backend's :run. Takes over the terminal, mounts the root component
  into a :window widget and pumps input until something calls quit! (or a
  configured quit key is pressed). Blocks, like every UI main loop.

  Options:
    :quit-keys     bindings that stop the loop (default ctrl-c and ctrl-q)
    :tick-ms       input poll interval (default 30). Also the worst-case delay
                   before a timer fires or work posted from another thread is
                   picked up.
    :auto-quit-ms  stop after roughly this long — for smoke tests, which have no
                   one to press a key.

  The terminal is restored in a finally, so a handler that throws still leaves a
  usable shell behind."
  [opts mount-root!]
  (let [{:keys [tick-ms auto-quit-ms quit-keys]
         :or {tick-ms 30 quit-keys #{"ctrl+c" "ctrl+q"}}} opts
        win (curses/start!)
        screen (curses/ncurses-screen win)
        root (w/node :window {})]
    (try
      (attach! root screen)
      (swap! app assoc :win win)
      (mount-root! root :window)
      (reset! b/loop-running? true)
      (loop [painted -1 elapsed 0]
        (drain!)
        (pump-timers!)
        (let [d @w/dirty
              painted (if (= d painted) painted (do (repaint!) d))]
          (when-let [event (read-event! win tick-ms)]
            (press! event quit-keys))
          ;; elapsed is counted in ticks rather than read from a clock: the loop
          ;; has no other need for time, and a tick is a good enough unit for the
          ;; auto-quit that smoke tests use.
          (let [elapsed (+ elapsed tick-ms)]
            (when-not (or (:quit? @app)
                          (and auto-quit-ms (>= elapsed auto-quit-ms)))
              (recur painted elapsed)))))
      (finally
        (reset! b/loop-running? false)
        (cancel-all!)
        (curses/stop!)
        (reset! app nil)))))

;; --- the backend -------------------------------------------------------------
(def backend
  "The terminal backend map handed to glimmer.backend/register!. See that
  namespace for the contract each key satisfies."
  {:name           :tui
   :create!        w/create!
   :apply-props!   w/apply-props!
   :append-child!  w/append-child!
   :remove-child!  w/remove-child!
   :replace-child! w/replace-child!
   :reorder-child! w/reorder-child!
   :schedule       schedule
   :run            run!})

(defn install!
  "Make the terminal the backend glimmer renders with. Called on load, so
  requiring this namespace is enough; exposed for code that wants to be explicit
  or to switch back after another backend was installed."
  []
  (b/register! backend)
  nil)

(defonce ^:private _installed (do (install!) true))

(defn run-async
  "Start `root-fn` on a background thread and return {:quit! f :result future}.

  `ui/run` blocks the calling thread, which makes it awkward to drive from a
  REPL. This does not — but it does mean the REPL and the UI are sharing one
  terminal, so anything that prints (including a stack trace) lands in the middle
  of the frame. Use tap> rather than println while a UI is up."
  [root-fn & {:as opts}]
  (let [run (requiring-resolve 'glimmer.core/run)
        result (future (apply run root-fn (mapcat identity (or opts {}))))]
    {:quit! quit! :result result}))
