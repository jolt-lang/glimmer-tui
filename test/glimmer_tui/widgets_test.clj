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
