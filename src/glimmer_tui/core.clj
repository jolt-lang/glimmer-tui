(ns glimmer-tui.core
  "The terminal backend for glimmer. Requiring this namespace installs it, after
  which glimmer's portable reconciler renders hiccup into a curses screen:

    (ns myapp
      (:require [glimmer.ratom :refer [atom]]
                [glimmer.core :as ui]
                [glimmer-tui.core]))          ; installs the ncurses backend

    (defn -main [& _] (ui/run my-app))

  What this namespace adds to the widget layer is the part a GUI toolkit would
  normally own: an event loop, keyboard focus and hit testing.

  Focus lives here rather than in glimmer's core because it is a property of how
  a toolkit is driven, not of the component model — GTK gets focus from GTK. The
  focus ring is recomputed from the widget tree on every repaint, in tree order,
  so a component that renders a new button gets a sensible tab position with no
  registration step. Focus follows the widget id, so a keyed list can reorder
  around the focused row without the focus jumping to a different item.

  Keys are offered to the focused widget first (an entry wants Space and the
  arrows), then to the loop's own bindings (Tab, Enter, quit). Mouse button 1
  focuses and activates whatever is under the pointer."
  (:require [glimmer.backend :as b]
            [glimmer-tui.curses :as curses]
            [glimmer-tui.ffi :as c]
            [glimmer-tui.layout :as layout]
            [glimmer-tui.render :as render]
            [glimmer-tui.screen :as scr]
            [glimmer-tui.widget :as w]))

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

;; --- painting ----------------------------------------------------------------
(defn paint!
  "Snapshot, lay out and paint the tree rooted at widget `root` onto `screen`.
  Returns the laid-out snapshot. Public because it is the whole render pipeline
  in one call: tests drive it against an in-memory screen, with no terminal."
  ([root screen] (paint! root screen nil))
  ([root screen focus-id]
   (let [[cols rows] (scr/size screen)
         tree (layout/layout (w/snapshot root) cols rows)]
     (render/render! screen tree {:focus-id focus-id})
     tree)))

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

(defn- resolve-focus
  "Keep the focus on a live widget: hold the current one while it is still in
  the ring, otherwise fall back to the first focusable widget (or nothing)."
  [ring focus-id]
  (if (and focus-id (some #(= % focus-id) ring))
    focus-id
    (first ring)))

(defn- repaint! []
  (let [{:keys [root screen focus-id]} @app
        [cols rows] (scr/size screen)
        tree (layout/layout (w/snapshot root) cols rows)
        ring (w/focus-ring tree)
        focus (resolve-focus ring focus-id)]
    (swap! app assoc :tree tree :ring ring :focus-id focus)
    (render/render! screen tree {:focus-id focus})
    nil))

(defn frame!
  "Lay out and paint the current tree, refreshing the focus ring. Returns the
  laid-out snapshot."
  []
  (repaint!)
  (:tree @app))

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

(defn quit!
  "Ask the event loop to stop. Safe from any thread and from a handler."
  []
  (swap! app assoc :quit? true)
  nil)

(def ^:private ENTER-KEYS #{10 13 343})
(def ^:private TAB 9)
(def ^:private SPACE 32)

(defn click!
  "Focus and activate whatever focusable widget covers the cell (x, y). Returns
  the widget id that took the click, or nil when the cell is dead space."
  [x y]
  (when-let [n (w/hit (:tree @app) x y)]
    (swap! app assoc :focus-id (:id n))
    (activate! (w/find-widget (:root @app) (:id n)))
    (:id n)))

(defn- handle-mouse! []
  (when-let [{:keys [x y bstate]} (curses/read-mouse)]
    ;; button 1 press or click only: those three bits mean the same thing in
    ;; both ncurses mouse ABI versions (see glimmer-tui.ffi).
    (when (pos? (bit-and bstate (bit-or c/BUTTON1-PRESSED c/BUTTON1-CLICKED)))
      (click! x y)))
  nil)

(defn press!
  "Feed key `code` to the UI, exactly as the event loop does: Tab and the quit
  keys are the loop's, everything else is offered to the focused widget first.
  `quit-keys` defaults to none, which is what a test wants."
  ([code] (press! code #{}))
  ([code quit-keys]
  (cond
    (= code c/KEY-RESIZE) (w/touch!)
    (= code c/KEY-MOUSE)  (handle-mouse!)
    (= code TAB)          (move-focus! 1)
    (= code c/KEY-BTAB)   (move-focus! -1)
    (contains? quit-keys code) (quit!)
    :else
    ;; the focused widget gets first refusal: an entry consumes printable
    ;; characters, Backspace and the arrows, and only what it declines falls
    ;; through to the loop's own bindings.
    (let [widget (focused-widget)
          spec (when widget (w/spec-for (:tag @widget)))
          handled? (when-let [k (:key spec)] (k widget code))]
      (when-not handled?
        (when (or (contains? ENTER-KEYS code) (= code SPACE))
          (activate! widget)))))
  nil))

;; --- the event loop ----------------------------------------------------------
(defn- run!
  "glimmer.backend's :run. Takes over the terminal, mounts the root component
  into a :window widget and pumps input until something calls quit! (or a
  configured quit key is pressed). Blocks, like every UI main loop.

  Options:
    :quit-keys     key codes that stop the loop (default ctrl-c and ctrl-q)
    :tick-ms       input poll interval (default 30). Also the worst-case delay
                   before work posted from another thread is picked up.
    :auto-quit-ms  stop after roughly this long — for smoke tests, which have no
                   one to press a key.

  The terminal is restored in a finally, so a handler that throws still leaves a
  usable shell behind."
  [opts mount-root!]
  (let [{:keys [tick-ms auto-quit-ms quit-keys]
         :or {tick-ms 30 quit-keys #{3 17}}} opts
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
        (let [d @w/dirty
              painted (if (= d painted) painted (do (repaint!) d))]
          (let [code (curses/read-key win tick-ms)]
            (when code (press! code quit-keys)))
          ;; elapsed is counted in ticks rather than read from a clock: the loop
          ;; has no other need for time, and a tick is a good enough unit for the
          ;; auto-quit that smoke tests use.
          (let [elapsed (+ elapsed tick-ms)]
            (when-not (or (:quit? @app)
                          (and auto-quit-ms (>= elapsed auto-quit-ms)))
              (recur painted elapsed)))))
      (finally
        (reset! b/loop-running? false)
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
