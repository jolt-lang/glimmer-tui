(ns glimmer-tui.todo
  "A task board in the terminal. Exercises a derived reaction (the counts), an
  entry with :on-change / :on-activate, checkbutton toggles, a framed list and
  keyed rows — rows are keyed by task id, so adding, completing and clearing
  reuse each row's widgets and the focus stays on the task you were on.

  Run: jolt todo. Tab moves, Enter or Space presses, ctrl-c quits."
  (:require [glimmer.ratom :as r :refer [atom reaction]]
            [glimmer.core :as ui]
            [glimmer-tui.core :as tui]))

(defn- task-row [state {:keys [id text done]}]
  [:hbox {:spacing 1}
   [:checkbutton {:label text :active done
                  :on-toggled #(swap! state update :tasks
                                      (fn [ts] (mapv (fn [t] (if (= id (:id t))
                                                               (update t :done not)
                                                               t))
                                                     ts)))}]])

(defn todo-app []
  (let [state (atom {:tasks [{:id 1 :text "Read the glimmer README" :done true}
                             {:id 2 :text "Toggle a task with space" :done false}
                             {:id 3 :text "Add one of your own"      :done false}]
                     :next-id 4})
        draft (atom "")
        left (reaction (count (remove :done (:tasks @state))))
        add (fn []
              (when (seq @draft)
                (swap! state (fn [s]
                               (-> s
                                   (update :tasks conj {:id (:next-id s)
                                                        :text @draft :done false})
                                   (update :next-id inc))))
                (reset! draft "")))]
    (fn []
      [:vbox {:spacing 1 :margin 1}
       [:label {:label "Tasks" :bold true}]

       [:frame {:label (str @left " remaining") :vexpand true}
        (into [:vbox {:spacing 0 :margin 1}]
              (if (empty? (:tasks @state))
                [[:label {:label "Nothing here yet — add a task below." :dim true}]]
                (for [t (:tasks @state)]
                  ^{:key (:id t)} [task-row state t])))]

       [:hbox {:spacing 1}
        [:entry {:text @draft :placeholder "Add a task…" :hexpand true
                 :on-change #(reset! draft %)
                 :on-activate add}]
        [:button {:label "Add" :on-click add}]
        [:button {:label "Clear done"
                  :on-click #(swap! state update :tasks (fn [ts] (vec (remove :done ts))))}]
        [:button {:label "Quit" :on-click tui/quit!}]]

       [:label {:label "tab: move   enter/space: press   ctrl-c: quit" :dim true}]])))

(defn -main [& _]
  (ui/run todo-app))
