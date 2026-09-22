(ns glimmer-tui.widgets-test
  "The widgets that carry state of their own — a list's cursor, a table's row, an
  entry's edit buffer — driven directly through their specs, with no event loop
  in the way."
  (:require [clojure.test :refer [deftest is testing]]
            [glimmer-tui.keys :as keys]
            [glimmer-tui.layout :as l]
            [glimmer-tui.render :as render]
            [glimmer-tui.screen :as scr]
            [glimmer-tui.widget :as w]
            [glimmer-tui.widgets]))

(defn- laid-out
  "A widget on its own, laid out into a cols x rows box — which is what a key
  handler needs, since how far Page Down goes depends on how tall the widget is."
  [wid cols rows]
  (l/layout (w/snapshot wid) cols rows))

(defn- press!
  "Send a key code (or an event) to `wid` as the loop would, with the laid-out
  node as context."
  ([wid code cols rows] (press! wid code cols rows {}))
  ([wid code cols rows extra]
   (let [event (if (map? code) code (keys/decode code))
         node (laid-out wid cols rows)]
     ((:key (w/spec-for (:tag @wid))) wid event (merge {:node node} extra)))))

(defn- paint
  "Paint one widget into a fresh screen and read the frame back."
  [wid cols rows]
  (let [screen (scr/buffer-screen cols rows)
        tree (laid-out wid cols rows)]
    (render/render! screen tree {})
    (scr/lines screen)))

(def ^:private DOWN 258)
(def ^:private UP 259)
(def ^:private END 360)
(def ^:private HOME 262)

;; --- listbox -----------------------------------------------------------------
(deftest a-list-moves-its-cursor-and-tells-the-component
  (let [seen (atom [])
        lb (w/create! :listbox {:items ["alpha" "beta" "gamma"]
                                :on-select (fn [i item] (swap! seen conj [i item]))})]
    (is (= ["> alpha" "  beta" "  gamma"] (paint lb 10 3)))
    (press! lb DOWN 10 3)
    (is (= 1 (:cursor (:state @lb))))
    (is (= [[1 "beta"]] @seen) ":on-select fires with the index and the item")
    (is (= ["  alpha" "> beta" "  gamma"] (paint lb 10 3)))
    (testing "and stops at the ends rather than wrapping"
      (press! lb UP 10 3)
      (press! lb UP 10 3)
      (is (= 0 (:cursor (:state @lb))))
      (press! lb END 10 3)
      (is (= 2 (:cursor (:state @lb))))
      (press! lb DOWN 10 3)
      (is (= 2 (:cursor (:state @lb)))))
    (testing "unless it was asked to wrap"
      (w/apply-props! :listbox lb {:items ["alpha" "beta" "gamma"] :wrap true})
      (press! lb DOWN 10 3)
      (is (= 0 (:cursor (:state @lb)))))))

(deftest a-list-scrolls-the-cursor-into-view-when-it-is-taller-than-its-box
  (let [items (mapv #(str "item " %) (range 8))
        lb (w/create! :listbox {:items items})]
    (is (= ["> item 0" "  item 1" "  item 2"] (paint lb 10 3)))
    (dotimes [_ 4] (press! lb DOWN 10 3))
    (is (= 4 (:cursor (:state @lb))))
    (is (= ["  item 2" "  item 3" "> item 4"] (paint lb 10 3))
        "the window followed the cursor down")
    (press! lb HOME 10 3)
    (is (= ["> item 0" "  item 1" "  item 2"] (paint lb 10 3)))))

(deftest a-list-selection-can-be-driven-from-props
  (let [lb (w/create! :listbox {:items ["a" "b" "c"] :selected 2})]
    (is (= 2 (:cursor (:state @lb))))
    (w/apply-props! :listbox lb {:items ["a" "b" "c"] :selected 0})
    (is (= 0 (:cursor (:state @lb))) "the component owns the selection when it says so")))

(deftest a-click-selects-the-row-under-the-pointer
  (let [seen (atom nil)
        lb (w/create! :listbox {:items ["a" "b" "c"]
                                :on-select (fn [i _] (reset! seen i))})
        node (laid-out lb 10 3)]
    ((:click (w/spec-for :listbox)) lb 1 2 {:node node})
    (is (= 2 @seen))))

;; --- table -------------------------------------------------------------------
(deftest a-table-sizes-its-columns-to-their-contents
  (let [t (w/create! :table {:columns [{:title "id" :key :id} {:title "name" :key :name}]
                             :rows [{:id 1 :name "ada"} {:id 22 :name "grace"}]})]
    (is (= ["id name" "1  ada" "22 grace"] (paint t 16 3)))))

(deftest a-table-moves-by-row-under-its-header
  (let [seen (atom nil)
        t (w/create! :table {:columns [{:title "n" :key :n}]
                             :rows [{:n "one"} {:n "two"} {:n "three"}]
                             :on-select (fn [i _] (reset! seen i))})]
    (press! t DOWN 10 4)
    (is (= 1 @seen))
    (press! t END 10 4)
    (is (= 2 (:cursor (:state @t))))
    (testing "a click lands on the row, not the header"
      ((:click (w/spec-for :table)) t 0 1 {:node (laid-out t 10 4)})
      (is (= 0 (:cursor (:state @t)))))))

(deftest a-header-can-be-turned-off
  (let [t (w/create! :table {:columns [{:title "n" :key :n}]
                             :rows [{:n "one"}]
                             :header false})]
    (is (= ["one"] (paint t 8 1)))))

(defn- painted-screen
  [wid cols rows focus?]
  (let [screen (scr/buffer-screen cols rows)
        tree (laid-out wid cols rows)]
    (render/render! screen tree (if focus? {:focus-id (:id @wid)} {}))
    screen))

(deftest table-row-and-cell-styles-resolve-with-documented-precedence
  (let [row-seen (atom [])
        cell-seen (atom [])
        table (w/create!
               :table
               {:columns [{:title "A" :key :a :width 3
                           :style (fn [context]
                                    (swap! cell-seen conj context)
                                    {:fg (if (= 0 (:column-index context)) :cyan :red)})}
                          {:title "B" :key :b :width 2 :style {:underline true}}]
                :rows [{:a "x" :b 7 :color :green}
                       {:a "y" :b 8 :color :yellow}]
                :header false
                :gap 2
                :fg :white
                :bg :black
                :row-style (fn [{:keys [row index] :as context}]
                             (swap! row-seen conj context)
                             {:fg (:color row) :dim (= index 1)})})
        screen (painted-screen table 10 2 false)]
    (is (= :cyan (:fg (scr/style-at screen 0 0))) "cell style overrides row style")
    (is (= :black (:bg (scr/style-at screen 0 0))) "table style remains the base")
    (is (= :green (:fg (scr/style-at screen 3 0))) "the gap uses row style")
    (is (= :green (:fg (scr/style-at screen 7 0))) "trailing fill uses row style")
    (is (:underline (scr/style-at screen 5 0)) "static cell style affects its column")
    (is (= :yellow (:fg (scr/style-at screen 3 1))) "row callbacks vary by row data")
    (is (:dim (scr/style-at screen 7 1)) "row callbacks receive the complete index")
    (is (= [0 1] (mapv :index @row-seen)))
    (is (= ["x" "y"] (mapv :value @cell-seen)))
    (is (= [0 0] (mapv :column-index @cell-seen)))
    (is (every? #(contains? % :selected?) (concat @row-seen @cell-seen)))
    (is (every? #(contains? % :focused?) (concat @row-seen @cell-seen)))))

(deftest table-static-row-style-and-selection-compose
  (let [props {:columns [{:title "A" :key :a :width 2 :style {:bg :blue}}]
               :rows [{:a "x"}]
               :header false
               :fg :white
               :row-style {:fg :green :underline true}}
        focused (painted-screen (w/create! :table props) 4 1 true)
        unfocused (painted-screen (w/create! :table props) 4 1 false)]
    (is (= {:fg :green :bg :blue :underline true :reverse true}
           (scr/style-at focused 0 0)))
    (is (= {:fg :green :underline true :reverse true}
           (scr/style-at focused 3 0)) "focused selection styles trailing fill")
    (is (= {:fg :green :bg :blue :underline true :bold true}
           (scr/style-at unfocused 0 0)))
    (is (= {:fg :green :underline true :bold true}
           (scr/style-at unfocused 3 0)) "unfocused selection retains row attributes")))

(deftest table-style-callbacks-use-complete-index-after-scrolling
  (let [seen (atom [])
        table (w/create! :table
                         {:columns [{:title "A" :key :a
                                    :style (fn [{:keys [index] :as context}]
                                             (swap! seen conj context)
                                             {:fg (if (= index 2) :cyan :red)})}]
                          :rows [{:a "zero"} {:a "one"} {:a "two"}]
                          :header false
                          :row-style (fn [context]
                                       (swap! seen conj context)
                                       {:bg :black})})]
    (dotimes [_ 2] (press! table DOWN 6 1))
    (let [screen (painted-screen table 6 1 true)]
      (is (= 2 (:offset (:state @table))))
      (is (= "two" (first (scr/lines screen))))
      (is (= :cyan (:fg (scr/style-at screen 0 0))))
      (is (= [2 2] (mapv :index @seen)))
      (is (= {:a "two"} (:row (last @seen)))))))

(deftest table-ignores-invalid-style-values
  (let [calls (atom 0)
        table (w/create! :table
                         {:columns [{:title "A" :key :a :width 2 :style :invalid}
                                    {:title "B" :key :b :width 2
                                     :style (fn [_] (swap! calls inc) :invalid)}]
                          :rows [{:a "x" :b "y"}]
                          :header false
                          :color :white
                          :row-style (fn [_] :invalid)})
        screen (painted-screen table 7 2 false)]
    (is (= :white (:fg (scr/style-at screen 0 0))))
    (is (= :white (:fg (scr/style-at screen 3 0))))
    (is (= 1 @calls))
    (is (= :white (:fg (scr/style-at screen 0 1)))
        "empty rows retain base style without callbacks")
    (is (= 1 @calls) "empty rows do not invoke cell callbacks")))

(deftest clipped-table-cells-keep-their-absolute-offsets
  (let [table (w/create! :table
                         {:columns [{:title "A" :key :a :width 3 :style {:fg :red}}
                                    {:title "B" :key :b :width 3 :style {:fg :blue}}]
                          :rows [{:a "abc" :b "XYZ"}]
                          :header false
                          :gap 1
                          :row-style {:fg :green}})
        screen (scr/buffer-screen 8 1)
        clipped (scr/clip screen {:x 2 :y 0 :w 5 :h 1})
        node (assoc (w/snapshot table) :rect {:x 0 :y 0 :w 8 :h 1})]
    ((:paint (w/spec-for :table)) clipped (:rect node) (:props node) node {})
    (is (= "c XYZ" (scr/text-at screen 2 0 5)))
    (is (= :red (:fg (scr/style-at screen 2 0))))
    (is (= :green (:fg (scr/style-at screen 3 0))) "the clipped gap keeps row style")
    (is (= :blue (:fg (scr/style-at screen 4 0)))
        "the later cell does not restart at the clip edge")))

;; --- entry -------------------------------------------------------------------
(defn- type! [e & codes]
  (doseq [c codes]
    (press! e (if (char? c) (int c) c) 20 1)))

(deftest an-entry-edits-by-word
  (let [e (w/create! :entry {:text "hello brave world"})]
    (type! e {:type :ctrl :ch \w})
    (is (= "hello brave " (:value (:state @e))) "ctrl-w rubs out the word before the caret")
    (type! e {:type :alt :ch \b :base-type :char})
    (is (= 6 (:cursor (:state @e))) "alt-b moves back a word")
    (type! e {:type :ctrl :ch \k})
    (is (= "hello " (:value (:state @e))) "ctrl-k cuts to the end")
    (type! e {:type :ctrl :ch \u})
    (is (= "" (:value (:state @e))) "ctrl-u cuts to the start")))

(deftest an-entry-respects-a-character-limit
  (let [e (w/create! :entry {:text "" :char-limit 3})]
    (apply type! e "abcdef")
    (is (= "abc" (:value (:state @e))))))

(deftest an-entry-can-hide-what-it-holds
  (let [e (w/create! :entry {:text "secret" :echo :password})]
    (is (= ["••••••"] (paint e 10 1)))
    (is (= "secret" (:value (:state @e))) "the value itself is untouched"))
  (let [e (w/create! :entry {:text "secret" :echo :none})]
    (is (= [""] (paint e 10 1)))))

(deftest an-entry-takes-a-paste-as-text-rather-than-as-keystrokes
  (let [e (w/create! :entry {:text "log: "})]
    (type! e (keys/paste "line one\nline two"))
    (is (= "log: line one line two" (:value (:state @e)))
        "the newline is a space, not an Enter and not a corrupted row")
    (is (= 22 (:cursor (:state @e))) "the caret lands after what was pasted"))
  (testing "a paste stops at the character limit like typing does"
    (let [e (w/create! :entry {:text "" :char-limit 4})]
      (type! e (keys/paste "abcdefgh"))
      (is (= "abcd" (:value (:state @e))))))
  (testing "a field can decide for itself what a paste means"
    (let [seen (atom nil)
          e (w/create! :entry {:text "keep" :on-paste #(reset! seen %)})]
      (type! e (keys/paste "two\nlines"))
      (is (= "two\nlines" @seen) "the handler is given the text as pasted")
      (is (= "keep" (:value (:state @e))) "and nothing is inserted behind it"))))

(deftest an-entry-scrolls-horizontally-to-keep-the-caret-in-view
  (let [e (w/create! :entry {:text "abcdefghij" :width-request 5})]
    (is (= ["fghij"] (paint e 5 1)) "the caret is at the end, so the tail shows")
    (type! e HOME)
    (is (= ["abcde"] (paint e 5 1)))))

;; --- indicators --------------------------------------------------------------
(deftest a-spinner-shows-the-frame-it-was-told-to
  (let [s (w/create! :spinner {:frames ["a" "b" "c"] :tick 4})]
    (is (= ["b"] (paint s 4 1)) "frame 4 of three frames is frame 1"))
  (let [s (w/create! :spinner {:frames ["x"] :label "working"})]
    (is (= ["x working"] (paint s 12 1)))))

(deftest a-paginator-shows-where-you-are
  (is (= ["○●○"] (paint (w/create! :paginator {:page 1 :total-pages 3}) 6 1)))
  (is (= ["2/3"] (paint (w/create! :paginator {:page 1 :total-pages 3 :style :arabic}) 6 1)))
  (testing "pages can be counted from the items instead"
    (is (= ["●○○"] (paint (w/create! :paginator {:page 0 :total-items 25 :per-page 10}) 6 1)))))

;; --- help --------------------------------------------------------------------
(deftest a-help-bar-renders-the-bindings-it-is-given
  (let [h (w/create! :help {:bindings [["q" "quit"] {:key "?" :desc "help"}]})]
    (is (= ["q quit  ? help"] (paint h 20 1)))))

(deftest a-help-bar-with-no-bindings-renders-what-is-focused
  (let [h (w/create! :help {})]
    (w/derive-state! h {:bindings [[:page-up ["pgup" "ctrl+b"]] [:quit ["q"]]]})
    (is (= ["pgup/ctrl+b page up  q quit"] (paint h 40 1)))
    (testing "and the action names can be relabelled"
      (w/apply-props! :help h {:labels {:quit "leave"}})
      (is (= ["pgup/ctrl+b page up  q leave"] (paint h 40 1))))))
