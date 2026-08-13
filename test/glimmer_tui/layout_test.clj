(ns glimmer-tui.layout-test
  "The box model, on plain data. No widgets, no screen, no terminal."
  (:require [clojure.test :refer [deftest is testing]]
            [glimmer-tui.layout :as l]))

(defn- n
  "A snapshot node, the shape glimmer-tui.widget/snapshot produces."
  ([tag props] (n tag props []))
  ([tag props kids] {:id (hash [tag props kids]) :tag tag :props props
                     :state {} :children (vec kids)}))

(defn- label [text & [props]] (n :label (merge {:label text} props)))

(defn- rects
  "Every node's :rect in tree order, so a whole arrangement is one assertion."
  [tree]
  (letfn [(walk [x] (cons (:rect x) (mapcat walk (:children x))))]
    (vec (walk tree))))

(deftest measures-a-label-from-its-text
  (let [m (l/measure (label "hello"))]
    (is (= {:w 5 :h 1} (:natural m))))
  (testing "a newline makes it taller and as wide as its widest line"
    (is (= {:w 5 :h 2} (:natural (l/measure (label "hi\nthere")))))))

(deftest margins-grow-the-natural-size
  ;; margin 2 is two cells on every side: 5+2+2 wide, 1+2+2 tall
  (is (= {:w 9 :h 5} (:natural (l/measure (label "hello" {:margin 2})))))
  (is (= {:w 7 :h 1} (:natural (l/measure (label "hello" {:margin-start 2})))))
  (testing "and shift the inner rect"
    (let [t (l/layout (label "hello" {:margin-start 2 :margin-top 1}) 20 5)]
      (is (= {:x 2 :y 1 :w 18 :h 4} (:rect t))))))

(deftest a-vertical-box-stacks-its-children
  (let [t (l/layout (n :box {:orientation :vertical}
                       [(label "one") (label "two") (label "three")])
                    10 5)]
    (is (= [{:x 0 :y 0 :w 10 :h 5}
            {:x 0 :y 0 :w 10 :h 1}
            {:x 0 :y 1 :w 10 :h 1}
            {:x 0 :y 2 :w 10 :h 1}]
           (rects t)))))

(deftest a-horizontal-box-lays-children-side-by-side
  (let [t (l/layout (n :box {:orientation :horizontal}
                       [(label "ab") (label "cde")])
                    10 3)]
    (is (= [{:x 0 :y 0 :w 10 :h 3}
            {:x 0 :y 0 :w 2 :h 3}
            {:x 2 :y 0 :w 3 :h 3}]
           (rects t)))))

(deftest spacing-separates-children
  (let [t (l/layout (n :box {:orientation :horizontal :spacing 2}
                       [(label "ab") (label "cd")])
                    10 1)]
    (is (= [{:x 0 :y 0 :w 2 :h 1} {:x 4 :y 0 :w 2 :h 1}]
           (rest (rects t))))))

(deftest expanders-share-the-leftover-space
  (testing "one expander takes it all"
    (let [t (l/layout (n :box {:orientation :horizontal}
                         [(label "ab") (label "cd" {:hexpand true})])
                      10 1)]
      (is (= [{:x 0 :y 0 :w 2 :h 1} {:x 2 :y 0 :w 8 :h 1}] (rest (rects t))))))
  (testing "two expanders split it, remainder to the first"
    (let [t (l/layout (n :box {:orientation :horizontal}
                         [(label "a" {:hexpand true}) (label "b" {:hexpand true})])
                      10 1)]
      (is (= [{:x 0 :y 0 :w 5 :h 1} {:x 5 :y 0 :w 5 :h 1}] (rest (rects t))))))
  (testing "with nobody expanding, the tail is left empty"
    (let [t (l/layout (n :box {:orientation :horizontal} [(label "ab") (label "cd")])
                      10 1)]
      (is (= [{:x 0 :y 0 :w 2 :h 1} {:x 2 :y 0 :w 2 :h 1}] (rest (rects t)))))))

(deftest children-are-clipped-when-the-box-is-too-small
  ;; a terminal cannot scroll what does not fit, so children are served in order
  ;; until the space runs out and the rest collapse to zero
  (let [t (l/layout (n :box {:orientation :horizontal}
                       [(label "aaaa") (label "bbbb") (label "cccc")])
                    6 1)]
    (is (= [{:x 0 :y 0 :w 4 :h 1} {:x 4 :y 0 :w 2 :h 1} {:x 6 :y 0 :w 0 :h 1}]
           (rest (rects t))))))

(deftest cross-axis-fills-by-default-and-aligns-on-request
  (testing "fill is the default, as in GTK"
    (let [t (l/layout (n :box {:orientation :vertical} [(label "ab")]) 10 3)]
      (is (= {:x 0 :y 0 :w 10 :h 1} (second (rects t))))))
  (testing ":halign :center centres the natural width"
    (let [t (l/layout (n :box {:orientation :vertical}
                         [(label "ab" {:halign :center})])
                      10 3)]
      (is (= {:x 4 :y 0 :w 2 :h 1} (second (rects t))))))
  (testing ":halign :end pushes it to the right edge"
    (let [t (l/layout (n :box {:orientation :vertical}
                         [(label "ab" {:halign :end})])
                      10 3)]
      (is (= {:x 8 :y 0 :w 2 :h 1} (second (rects t)))))))

(deftest a-frame-reserves-its-border
  (let [t (l/layout (n :frame {:label "title"} [(label "hi")]) 10 5)]
    (is (= {:w 4 :h 3} (:natural (l/measure (n :frame {} [(label "hi")]))))
        "border adds a cell on each side")
    (is (= [{:x 0 :y 0 :w 10 :h 5} {:x 1 :y 1 :w 8 :h 3}] (rects t)))))

(deftest width-request-sets-a-floor
  (is (= {:w 10 :h 1} (:natural (l/measure (label "ab" {:width-request 10})))))
  (is (= {:w 2 :h 4} (:natural (l/measure (label "ab" {:height-request 4}))))))
