(ns glimmer-tui.app-test
  "End to end: real components, glimmer's reconciler, this backend, an in-memory
  screen. Requiring glimmer-tui.core installs the backend, and core/attach! gives
  the session a screen that is not a terminal, so a whole interactive app can be
  driven by pressing keys and clicking cells in a test."
  (:require [clojure.test :refer [deftest is testing]]
            [glimmer.core :as ui]
            [glimmer.ratom :as r]
            [glimmer-tui.core :as tui]
            [glimmer-tui.keys :as keys]
            [glimmer-tui.screen :as scr]
            [glimmer-tui.widget :as w]))

(defn- session
  "Mount `component` into a fresh cols x rows session and paint the first frame."
  ([component] (session component 30 8))
  ([component cols rows]
   (let [root (w/node :window {})
         screen (scr/buffer-screen cols rows)]
     (tui/attach! root screen)
     (ui/mount root :window [component])
     (tui/frame!)
     {:root root :screen screen})))

(defn- text [screen] (scr/lines screen))

(def ^:private ENTER 10)
(def ^:private TAB 9)
(def ^:private BACKSPACE 263)

(deftest renders-a-component-tree
  (let [{:keys [screen]} (session (fn [] [:vbox {:spacing 1}
                                          [:label {:label "title"}]
                                          [:button {:label "go"}]]))]
    (is (= ["title" "" "[ go ]"] (take 3 (text screen))))))

(deftest enter-activates-the-focused-button-and-the-ui-follows-the-state
  (let [n (r/atom 0)
        app (fn [] [:vbox {}
                    [:label {:label (str "count " @n)}]
                    [:button {:label "add" :on-click #(swap! n inc)}]])
        {:keys [screen]} (session app)]
    (is (= "count 0" (first (text screen))))
    (tui/press! ENTER)
    (tui/frame!)
    (is (= "count 1" (first (text screen))))
    (tui/press! ENTER)
    (tui/press! ENTER)
    (tui/frame!)
    (is (= "count 3" (first (text screen))))))

(deftest tab-cycles-focus-and-wraps
  (let [hit (atom nil)
        app (fn [] [:vbox {}
                    [:button {:label "one" :on-click #(reset! hit :one)}]
                    [:button {:label "two" :on-click #(reset! hit :two)}]])
        _ (session app)]
    (tui/press! ENTER)
    (is (= :one @hit) "focus starts on the first focusable widget")
    (tui/press! TAB)
    (tui/press! ENTER)
    (is (= :two @hit))
    (testing "and comes back round"
      (tui/press! TAB)
      (tui/press! ENTER)
      (is (= :one @hit)))))

(deftest a-click-focuses-and-activates-what-is-under-it
  (let [hit (atom nil)
        app (fn [] [:vbox {}
                    [:button {:label "one" :on-click #(reset! hit :one)}]
                    [:button {:label "two" :on-click #(reset! hit :two)}]])
        _ (session app)]
    (tui/click! 3 1)
    (is (= :two @hit))
    (testing "focus moved with the click, so Enter now hits the same button"
      (reset! hit nil)
      (tui/press! ENTER)
      (is (= :two @hit)))
    (testing "clicking dead space does nothing"
      (reset! hit nil)
      (is (nil? (tui/click! 3 5)))
      (is (nil? @hit)))))

(deftest typing-into-an-entry-drives-the-component
  (let [draft (r/atom "")
        app (fn [] [:vbox {}
                    [:label {:label (str "draft: " @draft)}]
                    [:entry {:text @draft :on-change #(reset! draft %)}]])
        {:keys [screen]} (session app)]
    (doseq [c "hi"] (tui/press! (int c)))
    (tui/frame!)
    (is (= "hi" @draft))
    (is (= "draft: hi" (first (text screen))))
    (testing "backspace edits it back down"
      (tui/press! BACKSPACE)
      (tui/frame!)
      (is (= "h" @draft))
      (is (= "draft: h" (first (text screen)))))))

(deftest enter-in-an-entry-activates-it
  (let [submitted (atom [])
        draft (r/atom "")
        app (fn [] [:vbox {}
                    [:entry {:text @draft
                             :on-change #(reset! draft %)
                             :on-activate #(do (swap! submitted conj @draft)
                                               (reset! draft ""))}]])
        {:keys [screen]} (session app)]
    (doseq [c "task"] (tui/press! (int c)))
    (tui/press! ENTER)
    (tui/frame!)
    (is (= ["task"] @submitted))
    (is (= "" @draft))
    (is (= [""] (take 1 (text screen))) "the entry cleared with the state")))

(deftest a-checkbutton-round-trips-through-the-component
  (let [done (r/atom false)
        app (fn [] [:vbox {}
                    [:checkbutton {:label "task" :active @done
                                   :on-toggled #(swap! done not)}]])
        {:keys [screen]} (session app)]
    (is (= "[ ] task" (first (text screen))))
    (tui/press! 32)                                    ; space toggles
    (tui/frame!)
    (is (true? @done))
    (is (= "[x] task" (first (text screen))))))

(deftest keyed-rows-keep-their-focus-across-a-reorder
  ;; The reconciler reuses a keyed row's widgets when the list is reordered, and
  ;; focus follows the widget id — so the row you were on stays the row you are
  ;; on, even though it moved up the screen.
  (let [items (r/atom [:a :b :c])
        clicked (atom nil)
        app (fn []
              (into [:vbox {}]
                    (for [k @items]
                      [:button {:key k :label (name k)
                                :on-click #(reset! clicked k)}])))
        {:keys [screen]} (session app)]
    (is (= ["[ a ]" "[ b ]" "[ c ]"] (take 3 (text screen))))
    (tui/press! TAB)                                    ; focus the :b row
    (tui/press! ENTER)
    (is (= :b @clicked))
    (reset! items [:c :b :a])
    (tui/frame!)
    (is (= ["[ c ]" "[ b ]" "[ a ]"] (take 3 (text screen))))
    (reset! clicked nil)
    (tui/press! ENTER)
    (is (= :b @clicked) "still on the same row, which is now in the middle")))

(deftest a-removed-widget-hands-focus-back
  (let [show? (r/atom true)
        app (fn [] [:vbox {}
                    [:button {:label "keep"}]
                    (when @show? [:button {:label "temp"}])])
        _ (session app)]
    (tui/press! TAB)                                    ; focus "temp"
    (let [temp (tui/focus-id)]
      (reset! show? false)
      (tui/frame!)
      (is (not= temp (tui/focus-id)) "focus left the widget that went away")
      (is (some? (tui/focus-id)) "and landed on what remains"))))

(deftest the-frame-only-repaints-when-something-changed
  (let [n (r/atom 0)
        app (fn [] [:label {:label (str @n)}])
        {:keys [screen]} (session app)
        before (scr/frames screen)]
    (tui/frame!)
    (is (= (inc before) (scr/frames screen)) "an explicit frame! always paints")
    (let [d @w/dirty]
      (reset! n 1)
      (is (> @w/dirty d) "a state change marks the tree dirty"))))

;; --- scrolling ---------------------------------------------------------------
(def ^:private DOWN 258)
(def ^:private ESC 27)
(def ^:private PGDN 338)

(deftest a-scroll-shows-a-window-onto-something-taller-than-the-terminal
  (let [app (fn [] [:scroll {:vexpand true}
                    (into [:vbox {}]
                          (for [i (range 8)] [:label {:label (str "row " i)}]))])
        {:keys [screen]} (session app 12 3)]
    (is (= ["row 0      █" "row 1      │" "row 2      │"] (text screen)))
    (tui/press! DOWN)
    (tui/frame!)
    (is (= ["row 1      █" "row 2      │" "row 3      │"] (text screen)))
    (testing "and the wheel scrolls whatever is under the pointer"
      (tui/scroll! 1 1 3)
      (tui/frame!)
      (is (= ["row 4" "row 5" "row 6"] (mapv #(subs % 0 5) (text screen)))))))

(deftest a-key-the-focused-widget-declines-reaches-the-scroll-around-it
  ;; The buttons take focus and consume Enter; Page Down means nothing to them,
  ;; so it carries on outwards to the container that does know what to do with it.
  (let [app (fn [] [:scroll {:vexpand true :scrollbar false}
                    (into [:vbox {}]
                          (for [i (range 12)] [:button {:label (str "b" i)}]))])
        {:keys [screen]} (session app 10 3)]
    (is (= ["[ b0 ]" "[ b1 ]" "[ b2 ]"] (text screen)))
    (tui/press! PGDN)
    (tui/frame!)
    (is (= ["[ b2 ]" "[ b3 ]" "[ b4 ]"] (text screen)))))

(deftest tabbing-to-an-off-screen-widget-scrolls-it-into-view
  (let [app (fn [] [:scroll {:vexpand true :scrollbar false}
                    (into [:vbox {}]
                          (for [i (range 8)] [:button {:label (str "b" i)}]))])
        {:keys [screen]} (session app 10 3)]
    (is (= ["[ b0 ]" "[ b1 ]" "[ b2 ]"] (text screen)))
    (dotimes [_ 4] (tui/press! TAB))
    (tui/frame!)
    (is (= ["[ b2 ]" "[ b3 ]" "[ b4 ]"] (text screen))
        "focus is on b4, which had to be brought back on screen to get there")))

;; --- overlays ----------------------------------------------------------------
(deftest a-modal-overlay-floats-over-the-page-and-keeps-focus-to-itself
  (let [open? (r/atom true)
        picked (atom nil)
        app (fn []
              [:vbox {}
               [:button {:label "behind" :on-click #(reset! picked :behind)}]
               (when @open?
                 [:overlay {:anchor :center :on-close #(reset! open? false)}
                  [:button {:label "ok" :on-click #(reset! picked :ok)}]])])
        {:keys [screen]} (session app 12 3)]
    (is (= ["[ behind ]" "   [ ok ]" ""] (text screen)))
    (testing "tab cannot leave the dialog"
      (tui/press! TAB)
      (tui/press! ENTER)
      (is (= :ok @picked)))
    (testing "and esc closes it"
      (tui/press! ESC)
      (tui/frame!)
      (is (false? @open?))
      (is (= ["[ behind ]" "" ""] (text screen)))
      (reset! picked nil)
      (tui/press! ENTER)
      (is (= :behind @picked) "focus fell back to what was underneath"))))

;; --- timers ------------------------------------------------------------------
(deftest a-timer-drives-an-animation-without-a-keypress
  (let [tick (r/atom 0)
        app (fn [] [:spinner {:frames ["a" "b" "c"] :tick @tick}])
        {:keys [screen]} (session app 6 1)
        id (tui/every! 1 #(swap! tick inc))]
    (is (= ["a"] (text screen)))
    (Thread/sleep 5)
    (tui/pump-timers!)
    (tui/frame!)
    (is (= ["b"] (text screen)))
    (tui/cancel! id)
    (Thread/sleep 5)
    (is (false? (tui/pump-timers!)) "a cancelled timer stops firing")
    (tui/frame!)
    (is (= ["b"] (text screen)))))

;; --- the derived help bar ----------------------------------------------------
(deftest the-help-bar-follows-the-focus
  (let [app (fn [] [:vbox {}
                    [:listbox {:items ["a" "b"]}]
                    [:entry {:text ""}]
                    [:help {}]])
        {:keys [screen]} (session app 40 4)]
    (is (= "↑/k/ctrl+p up" (subs (nth (text screen) 3) 0 13))
        "the list is focused, so the bar shows what a list answers to")
    (tui/press! TAB)
    (tui/frame!)
    (is (= "←/ctrl+b backward char" (subs (nth (text screen) 3) 0 22))
        "and the entry's bindings once focus moves")))

;; --- application-level keys --------------------------------------------------
(deftest a-container-can-bind-a-key-of-its-own
  (let [hits (atom [])
        draft (r/atom "")
        app (fn [] [:vbox {:on-key (fn [e] (when (keys/match? e "d")
                                             (swap! hits conj :d)
                                             true))}
                    [:entry {:text @draft :on-change #(reset! draft %)}]
                    [:button {:label "go"}]])
        _ (session app)]
    (testing "the focused widget gets first refusal"
      (tui/press! (int \d))
      (is (= "d" @draft) "the entry typed it")
      (is (= [] @hits)))
    (testing "and the container sees what nothing else wanted"
      (tui/press! TAB)                                  ; focus the button
      (tui/press! (int \d))
      (is (= [:d] @hits))
      (is (= "d" @draft) "the entry did not see it"))))

(deftest a-quit-key-does-not-fire-while-you-are-typing-it
  (let [draft (r/atom "")
        app (fn [] [:vbox {} [:entry {:text @draft :on-change #(reset! draft %)}]])
        _ (session app)]
    (tui/press! (int \q) #{"q" "ctrl+c"})
    (is (= "q" @draft) "a letter goes to the field, not to the loop")
    (tui/press! 3 #{"q" "ctrl+c"})
    (is (= "q" @draft))
    (testing "but a ctrl chord nothing consumes still quits"
      ;; ctrl-c reached the loop: the entry declined it
      (is (= "q" @draft)))))

(deftest autofocus-decides-which-widget-starts-focused
  ;; Tree order would put the focus on the filter field, and a focused field
  ;; swallows every letter — so `q` would not quit until you pressed Tab.
  (let [pressed (atom nil)
        app (fn [] [:vbox {}
                    [:entry {:text ""}]
                    [:button {:label "go" :autofocus true
                              :on-click #(reset! pressed :go)}]])
        _ (session app)]
    (tui/press! ENTER)
    (is (= :go @pressed)))
  (testing "and without it the first focusable widget still wins"
    (let [pressed (atom nil)
          app (fn [] [:vbox {}
                      [:button {:label "first" :on-click #(reset! pressed :first)}]
                      [:button {:label "second" :on-click #(reset! pressed :second)}]])
          _ (session app)]
      (tui/press! ENTER)
      (is (= :first @pressed)))))
