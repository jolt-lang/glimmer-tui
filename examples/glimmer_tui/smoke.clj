(ns glimmer-tui.smoke
  "Non-interactive proof that the real ncurses path works: mount a component into
  a live terminal, drive it from another thread, and auto-quit.

  Two things are checked, both of which only the real loop can show:

    a ratom mutated on a worker thread repaints (the change is marshalled onto
    the loop thread through the backend's :schedule, never painted from the
    worker), and

    a key posted onto the loop activates the focused button, which exercises
    focus, dispatch and the handler round trip through the reconciler.

  Exit 0 printing SMOKE OK means library load, locale, initscr, layout, paint,
  input dispatch and a clean endwin all worked. Needs a tty: under a pipe it
  prints SMOKE SKIP and exits 0, since there is nothing to test."
  (:require [glimmer.backend :as b]
            [glimmer.core :as ui]
            [glimmer.ratom :as r]
            [glimmer-tui.core :as tui]
            [glimmer-tui.ffi :as c]))

(def ticks (r/atom 0))
(def presses (r/atom 0))

(defn app []
  [:vbox {:spacing 1 :margin 1}
   [:label {:label "glimmer-tui smoke" :bold true}]
   [:label {:label (str "ticks from a worker thread: " @ticks)}]
   [:button {:label (str "pressed " @presses) :on-click #(swap! presses inc)}]])

(defn- drive!
  "Stand in for a user and for an nREPL session: mutate a cell from a worker
  thread, then post a keypress onto the loop thread."
  []
  (future
    (Thread/sleep 300)
    (swap! ticks inc)                       ; off-thread repaint
    (Thread/sleep 200)
    (b/schedule (fn [] (tui/press! 10)))    ; Enter on the focused button
    (Thread/sleep 200)
    (b/schedule (fn [] (tui/press! 10)))))

(defn -main [& _]
  (if (zero? (c/isatty 1))
    (println "SMOKE SKIP (no tty)")
    (try
      (drive!)
      (ui/run app :auto-quit-ms 1500)
      (let [ok? (and (= 1 @ticks) (= 2 @presses))]
        (println (if ok? "SMOKE OK" "SMOKE FAIL")
                 "ticks:" @ticks "presses:" @presses)
        (when-not ok?
          (when-let [exit (resolve 'jolt.host/exit)] (exit 1))))
      (catch :default e
        (println "SMOKE FAIL:" (ex-message e))
        (when-let [exit (resolve 'jolt.host/exit)] (exit 1))))))
