(ns glimmer-tui.widget-test
  "The widget layer on its own: prop application, the edit state an entry owns,
  the focus ring and hit testing."
  (:require [clojure.test :refer [deftest is testing]]
            [glimmer-tui.layout :as l]
            [glimmer-tui.widget :as w]))

(defn- tree [root] (w/snapshot root))

(deftest create-and-patch-are-pure-data
  (let [b (w/create! :button {:label "go"})]
    (is (= :button (:tag @b)))
    (is (= "go" (:label (:props @b))))
    (w/apply-props! :button b {:label "stop"})
    (is (= "stop" (:label (:props @b))))))

(deftest hbox-and-vbox-carry-their-orientation
  (is (= :horizontal (:orientation (:props @(w/create! :hbox {})))))
  (is (= :vertical (:orientation (:props @(w/create! :vbox {})))))
  (testing "an explicit orientation wins"
    (is (= :vertical (:orientation (:props @(w/create! :hbox {:orientation :vertical})))))))

(deftest children-are-appended-removed-replaced-and-reordered
  (let [box (w/create! :vbox {})
        a (w/create! :label {:label "a"})
        b (w/create! :label {:label "b"})
        c (w/create! :label {:label "c"})
        labels #(mapv (fn [x] (:label (:props @x))) (:children @box))]
    (w/append-child! :box box a)
    (w/append-child! :box box b)
    (is (= ["a" "b"] (labels)))
    (w/replace-child! :box box b c)
    (is (= ["a" "c"] (labels)))
    (w/append-child! :box box b)
    (w/reorder-child! :box box b nil)          ; nil sibling = move to the front
    (is (= ["b" "a" "c"] (labels)))
    (w/reorder-child! :box box b a)            ; after a
    (is (= ["a" "b" "c"] (labels)))
    (w/remove-child! :box box a)
    (is (= ["b" "c"] (labels)))))

(deftest every-mutation-marks-the-tree-dirty
  ;; the event loop only repaints when this counter moves, so a widget operation
  ;; that forgets to bump it would leave the screen stale
  (let [before @w/dirty
        b (w/create! :label {:label "x"})]
    (is (> @w/dirty before))
    (let [mid @w/dirty]
      (w/apply-props! :label b {:label "y"})
      (is (> @w/dirty mid)))))

;; --- entry state -------------------------------------------------------------
(defn- type-into! [entry & codes]
  (let [k (:key (w/spec-for :entry))]
    (doseq [c codes] (k entry (if (char? c) (int c) c)))))

(deftest an-entry-owns-its-edit-buffer
  (let [seen (atom [])
        e (w/create! :entry {:text "" :on-change #(swap! seen conj %)})]
    (type-into! e \a \b)
    (is (= "ab" (:value (:state @e))))
    (is (= 2 (:cursor (:state @e))))
    (is (= ["a" "ab"] @seen) ":on-change fires with the new text")
    (testing "backspace deletes before the caret"
      (type-into! e 263)
      (is (= "a" (:value (:state @e)))))
    (testing "arrows move the caret and insert in the middle"
      (type-into! e \z 260 \y)                 ; "az", left, insert y
      (is (= "ayz" (:value (:state @e)))))))

(deftest a-re-render-does-not-clobber-what-is-being-typed
  ;; The component re-renders on every keystroke (its :on-change wrote to a
  ;; ratom), so apply-props! arrives with the same text the widget already has.
  ;; If that overwrote the buffer, the caret would jump to the end and editing in
  ;; the middle of a word would be impossible.
  (let [e (w/create! :entry {:text ""})]
    (type-into! e \a \b \c 260 260)            ; "abc", caret back to index 1
    (is (= 1 (:cursor (:state @e))))
    (w/apply-props! :entry e {:text "abc"})
    (is (= "abc" (:value (:state @e))))
    (is (= 1 (:cursor (:state @e))) "caret survived the re-render")
    (testing "but a genuinely new value from the component does replace it"
      (w/apply-props! :entry e {:text "reset"})
      (is (= "reset" (:value (:state @e))))
      (is (= 5 (:cursor (:state @e)))))))

;; --- focus and hit testing ---------------------------------------------------
(deftest the-focus-ring-is-tree-order-over-focusable-widgets
  (let [box (w/create! :vbox {})
        lbl (w/create! :label {:label "not focusable"})
        b1 (w/create! :button {:label "one"})
        b2 (w/create! :checkbutton {:label "two"})
        e (w/create! :entry {})]
    (doseq [c [lbl b1 b2 e]] (w/append-child! :box box c))
    (is (= [(:id @b1) (:id @b2) (:id @e)] (w/focus-ring (tree box)))))
  (testing "an insensitive widget is skipped, so tab never lands on it"
    (let [box (w/create! :vbox {})
          b1 (w/create! :button {:label "on"})
          b2 (w/create! :button {:label "off" :sensitive false})]
      (w/append-child! :box box b1)
      (w/append-child! :box box b2)
      (is (= [(:id @b1)] (w/focus-ring (tree box)))))))

(deftest hit-finds-the-focusable-widget-under-a-cell
  (let [box (w/create! :vbox {})
        b1 (w/create! :button {:label "one"})
        b2 (w/create! :button {:label "two"})]
    (w/append-child! :box box b1)
    (w/append-child! :box box b2)
    (let [t (l/layout (tree box) 20 4)]
      (is (= (:id @b1) (:id (w/hit t 2 0))))
      (is (= (:id @b2) (:id (w/hit t 2 1))))
      (is (nil? (w/hit t 2 3)) "empty space takes no click"))))

(deftest find-widget-reaches-the-live-atom
  (let [box (w/create! :vbox {})
        b (w/create! :button {:label "go"})]
    (w/append-child! :box box b)
    (is (= b (w/find-widget box (:id @b))))
    (is (nil? (w/find-widget box -1)))))
