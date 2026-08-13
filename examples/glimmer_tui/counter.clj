(ns glimmer-tui.counter
  "A reactive counter — the canonical reagent-style demo, in a terminal.

  Local state lives in a reactive atom created once (Form-2 component). Pressing
  a button swaps the atom; glimmer re-renders just this component and the next
  frame repaints the label in place. Tab moves between buttons, Enter or Space
  presses one, ctrl-c quits. Try `jolt counter`.

  Nothing in the component below is terminal-specific: the same code renders
  through glimmer-gtk as GTK widgets."
  (:require [glimmer.ratom :as r :refer [atom]]
            [glimmer.core :as ui]
            [glimmer-tui.core :as tui]))

(defn counter
  "Form-2: outer fn creates state once, inner fn renders."
  []
  (let [count (atom 0)]
    (fn []
      [:vbox {:spacing 1 :margin 2}
       [:label {:label (str "Count: " @count) :bold true}]
       [:hbox {:spacing 2}
        [:button {:label "− 1" :on-click #(swap! count dec)}]
        [:button {:label "+ 1" :on-click #(swap! count inc)}]
        [:button {:label "reset" :on-click #(reset! count 0)}]
        [:button {:label "quit" :on-click tui/quit!}]]
       [:label {:label "tab: move   enter/space: press   ctrl-c: quit" :dim true}]])))

(defn -main [& _]
  (ui/run counter))
