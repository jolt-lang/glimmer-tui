(ns glimmer-tui.layout-test
  "The box model, on plain data. No widgets, no screen, no terminal."
  (:require [clojure.test :refer [deftest is testing]]
            [glimmer-tui.layout :as l]
            [glimmer-tui.widgets]))

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

(deftest padding-grows-a-container-and-insets-its-children
  (is (= {:w 9 :h 5} (:natural (l/measure (n :vbox {:padding 2} [(label "hello")]))))
      "padding counts towards the natural size, like a margin")
  (testing "the shorthand is CSS's"
    (is (= {:w 9 :h 3} (:natural (l/measure (n :vbox {:padding [1 2]} [(label "hello")])))))
    ;; [top right bottom left], as in CSS: 5+2+4 wide, 1+1+3 tall
    (is (= {:w 11 :h 5} (:natural (l/measure (n :vbox {:padding [1 2 3 4]}
                                                [(label "hello")]))))))
  (testing "and the child is placed inside it, while the box keeps the whole rect"
    (let [t (l/layout (n :vbox {:padding 1} [(label "hi")]) 10 4)]
      (is (= {:x 0 :y 0 :w 10 :h 4} (:rect t)) "the box still owns its background")
      (is (= {:x 1 :y 1 :w 8 :h 1} (:rect (first (:children t))))))))

(deftest a-scroll-lays-its-child-out-at-full-size-and-offsets-it
  (let [tall (n :vbox {} (repeat 10 (label "row")))
        node (assoc (n :scroll {} [tall]) :state {:y-offset 3})
        t (l/layout node 12 4)]
    (is (= {:w 12 :h 4} (select-keys (:rect t) [:w :h])))
    (is (= {:w 11 :h 4} (:viewport t)) "a column goes to the scrollbar")
    (is (= 10 (:h (:content t))) "the content is as tall as the child wants to be")
    (is (= {:x 0 :y 3} (:offset t)))
    (is (= -3 (:y (:rect (first (:children t)))))
        "the child starts above the viewport; the renderer clips it")))

(deftest a-scroll-clamps-an-offset-past-the-end
  (let [tall (n :vbox {} (repeat 6 (label "row")))
        node (assoc (n :scroll {} [tall]) :state {:y-offset 99})
        t (l/layout node 12 4)]
    (is (= 2 (:y (:offset t))) "six rows in four, so two is as far as it goes")))

(deftest a-scroll-that-fits-does-not-scroll
  (let [short (n :vbox {} [(label "a") (label "b")])
        node (assoc (n :scroll {} [short]) :state {:y-offset 5})
        t (l/layout node 12 8)]
    (is (= 0 (:y (:offset t))))
    (is (= 8 (:h (:content t))) "the content is at least the viewport")))

(deftest an-overlay-takes-no-room-and-is-placed-against-the-screen
  (let [tree (n :vbox {} [(label "under")
                          (n :overlay {:anchor :center} [(label "hi")])])
        m (l/measure tree)]
    (is (= {:w 5 :h 1} (:natural m)) "the overlay adds nothing to its parent")
    (let [t (l/layout tree 11 5)
          o (second (:children t))]
      (is (= {:x 4 :y 2 :w 2 :h 1} (:rect o)) "centred on the screen, not on the box"))))

(deftest overlay-anchors
  (let [place (fn [anchor]
                (-> (n :window {} [(n :overlay {:anchor anchor} [(label "ab")])])
                    (l/layout 10 4)
                    :children first :rect
                    (select-keys [:x :y])))]
    (is (= {:x 0 :y 0} (place :top-left)))
    (is (= {:x 8 :y 0} (place :top-right)))
    (is (= {:x 0 :y 3} (place :bottom-left)))
    (is (= {:x 8 :y 3} (place :bottom-right)))
    (is (= {:x 4 :y 1} (place :center)))))

;; --- minimums and shrinking --------------------------------------------------
(deftest a-node-reports-what-it-can-live-on-as-well-as-what-it-wants
  (testing "a label cannot be shorter than its text"
    (let [m (l/measure (label "hi\nthere"))]
      (is (= (:natural m) (:min m)))))
  (testing "a scroll can be given nothing at all"
    (let [m (l/measure (n :scroll {} [(n :vbox {} (repeat 9 (label "row")))]))]
      (is (= 9 (:h (:natural m))))
      (is (= 0 (:h (:min m))))))
  (testing "a list can live in one row, and a table in a header plus one"
    (is (= 1 (:h (:min (l/measure (n :listbox {:items ["a" "b" "c"]}))))))
    (is (= 2 (:h (:min (l/measure (n :table {:columns [{:title "c" :key :c}]
                                             :rows [{:c 1} {:c 2}]})))))))
  (testing "a size request is a floor on the minimum too"
    (is (= 4 (:h (:min (l/measure (n :scroll {:height-request 4}
                                     [(n :vbox {} (repeat 9 (label "row")))]))))))))

(deftest what-can-shrink-shrinks-before-anything-is-clipped
  ;; The whole point of a scroll: nine rows of content in a box six tall must not
  ;; cost the footer its row.
  (let [tree (l/layout (n :vbox {} [(label "header")
                                    (n :scroll {} [(n :vbox {} (repeat 9 (label "row")))])
                                    (label "footer")])
                       10 6)
        [header scroll footer] (:children tree)]
    (is (= 1 (:h (:rect header))))
    (is (= 4 (:h (:rect scroll))) "the scroll gave up the five rows that were missing")
    (is (= 1 (:h (:rect footer))) "and the footer kept its own")))

(deftest the-squeeze-is-shared-in-proportion-to-what-each-can-give
  (let [scroll (fn [rows] (n :scroll {:scrollbar false}
                             [(n :vbox {} (repeat rows (label "row")))]))
        tree (l/layout (n :vbox {} [(scroll 10) (scroll 20)]) 10 12)
        [a b] (:children tree)]
    ;; 30 rows of content in 12: 18 to give up, split 1:2
    (is (= 4 (:h (:rect a))))
    (is (= 8 (:h (:rect b))))
    (is (= 12 (+ (:h (:rect a)) (:h (:rect b)))) "and the total lands exactly")))

(deftest when-even-the-minimums-do-not-fit-children-are-served-in-order
  (let [tree (l/layout (n :vbox {} [(label "a\nb\nc") (label "d\ne") (label "f")]) 10 4)
        [a b c] (:children tree)]
    (is (= 3 (:h (:rect a))))
    (is (= 1 (:h (:rect b))) "what was left")
    (is (= 0 (:h (:rect c))) "and nothing for the tail")))
