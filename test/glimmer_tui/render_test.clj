(ns glimmer-tui.render-test
  "Painting, asserted by reading the rendered frame back as text. The screen is
  glimmer-tui.screen/buffer-screen, so these run with no terminal."
  (:require [clojure.test :refer [deftest is testing]]
            [glimmer-tui.layout :as l]
            [glimmer-tui.render :as r]
            [glimmer-tui.screen :as scr]
            [glimmer-tui.widget :as w]
            [glimmer-tui.widgets]))

(defn- paint
  "Build a widget tree from `spec` ([tag props & children], recursively), paint
  it onto a fresh cols x rows screen, and return the screen."
  ([spec cols rows] (paint spec cols rows nil))
  ([spec cols rows focus-tag]
   (let [screen (scr/buffer-screen cols rows)
         build (fn build [[tag props & kids]]
                 (let [node (w/create! tag props)]
                   (doseq [k kids] (w/append-child! tag node (build k)))
                   node))
         root (build spec)
         tree (l/layout (w/snapshot root) cols rows)
         focus (when focus-tag
                 (:id (first (filter #(= focus-tag (:tag %)) (w/walk tree)))))]
     (r/render! screen tree {:focus-id focus})
     screen)))

(deftest paints-a-label
  (let [s (paint [:label {:label "hello"}] 10 2)]
    (is (= ["hello" ""] (scr/lines s)))))

(deftest paints-a-vertical-stack-with-spacing
  (let [s (paint [:vbox {:spacing 1}
                  [:label {:label "one"}]
                  [:label {:label "two"}]]
                 10 4)]
    (is (= ["one" "" "two" ""] (scr/lines s)))))

(deftest paints-a-horizontal-row
  (let [s (paint [:hbox {:spacing 2}
                  [:label {:label "ab"}]
                  [:label {:label "cd"}]]
                 10 1)]
    (is (= ["ab  cd"] (scr/lines s)))))

(deftest a-button-is-bracketed-and-a-checkbutton-shows-its-state
  (is (= ["[ go ]"] (scr/lines (paint [:button {:label "go"}] 10 1))))
  (is (= ["[ ] task"] (scr/lines (paint [:checkbutton {:label "task"}] 10 1))))
  (is (= ["[x] task"] (scr/lines (paint [:checkbutton {:label "task" :active true}] 10 1)))))

(deftest the-focused-widget-paints-in-reverse-video
  (let [focused (paint [:vbox {} [:button {:label "go"}]] 10 1 :button)
        plain   (paint [:vbox {} [:button {:label "go"}]] 10 1)]
    (is (= ["[ go ]"] (scr/lines focused)))
    (is (:reverse (scr/style-at focused 0 0)) "the focused button is reversed")
    (is (nil? (:reverse (scr/style-at plain 0 0))) "an unfocused one is not")))

(deftest a-frame-draws-a-border-and-its-title
  (let [s (paint [:frame {:label "box"} [:label {:label "hi"}]] 8 4)]
    (is (= ["┌ box ─┐"
            "│hi    │"
            "│      │"
            "└──────┘"]
           (scr/lines s)))))

(deftest children-are-clipped-to-their-parent
  ;; without clipping, this label would paint straight through the frame's right
  ;; border and out the other side
  (let [s (paint [:frame {} [:label {:label "aaaaaaaaaaaaaaaa"}]] 8 3)]
    (is (= ["┌──────┐"
            "│aaaaaa│"
            "└──────┘"]
           (scr/lines s)))))

(deftest an-entry-shows-its-placeholder-until-it-has-text
  (let [s (paint [:entry {:placeholder "name…" :width-request 8}] 10 1)]
    (is (= ["name…"] (scr/lines s)))
    (is (:dim (scr/style-at s 0 0)) "the placeholder is dimmed")))

(deftest the-cursor-sits-in-the-focused-entry-and-nowhere-else
  (let [s (paint [:vbox {} [:entry {:text "abc"}]] 10 1 :entry)]
    (is (= [3 0] (scr/cursor s)) "at the end of the text"))
  (let [s (paint [:vbox {} [:button {:label "go"}]] 10 1 :button)]
    (is (nil? (scr/cursor s)) "a button hides it")))

(deftest wide-glyphs-occupy-two-cells
  (let [s (paint [:hbox {} [:label {:label "日本"}] [:label {:label "x"}]] 10 1)]
    (is (= ["日本x"] (scr/lines s)))
    (is (= "x" (scr/text-at s 4 0 1)) "the x starts in column 4, not 2")))

(deftest a-separator-fills-its-run
  (is (= ["────"] (scr/lines (paint [:separator {:hexpand true}] 4 1)))))

(deftest margins-move-a-widget-in
  (let [s (paint [:label {:label "x" :margin-start 3 :margin-top 1}] 6 3)]
    (is (= ["" "   x" ""] (scr/lines s)))))

(deftest a-frame-draws-the-border-set-it-was-given
  (is (= ["╭──╮" "│hi│" "╰──╯"]
         (scr/lines (paint [:frame {:border :rounded} [:label {:label "hi"}]] 4 3))))
  (is (= ["+--+" "|hi|" "+--+"]
         (scr/lines (paint [:frame {:border :ascii} [:label {:label "hi"}]] 4 3))))
  (testing ":none gives the two cells back to the child"
    (is (= ["hi"] (scr/lines (paint [:frame {:border :none} [:label {:label "hi"}]] 4 1)))))
  (testing "a title can sit anywhere along the top edge"
    (is (= ["┌── t ┐"] (take 1 (scr/lines
                                 (paint [:frame {:label "t" :label-align :end}
                                         [:label {:label ""}]] 7 3)))))))

(deftest a-partly-scrolled-row-paints-where-it-belongs
  ;; The label is two lines tall and the viewport shows one; without clipping
  ;; that keeps the node's own coordinates, the second line would be redrawn at
  ;; the top of the clip instead of being the line that is visible.
  (let [s (paint [:scroll {:scrollbar false :height-request 1 :y-offset 1}
                  [:label {:label "first\nsecond"}]]
                 8 1)]
    (is (= ["second"] (scr/lines s)))))

(deftest a-scroll-draws-a-scrollbar-when-it-has-more-than-it-can-show
  (let [s (paint [:scroll {:height-request 3}
                  [:vbox {} [:label {:label "a"}] [:label {:label "b"}]
                   [:label {:label "c"}] [:label {:label "d"}]
                   [:label {:label "e"}] [:label {:label "f"}]]]
                 4 3)]
    (is (= ["a  █" "b  │" "c  │"] (scr/lines s))))
  (testing "and none when everything fits"
    (let [s (paint [:scroll {:height-request 3}
                    [:vbox {} [:label {:label "a"}]]]
                   4 3)]
      (is (= ["a"] (take 1 (scr/lines s)))))))

(deftest an-overlay-paints-over-everything-and-erases-what-it-covers
  (let [s (paint [:vbox {}
                  [:label {:label "aaaaaa"}]
                  [:label {:label "bbbbbb"}]
                  [:overlay {:anchor :center} [:label {:label "XX"}]]]
                 6 3)]
    (is (= ["aaaaaa" "bbXXbb" ""] (scr/lines s))
        "the panel erased the cells it covers, and only those")))

(deftest a-progress-bar-fills-in-proportion
  (is (= ["████░░░░"] (scr/lines (paint [:progress {:value 0.5 :halign :start
                                                    :width-request 8}] 8 1))))
  (is (= ["░░░░░░░░"] (scr/lines (paint [:progress {:value 0 :halign :start
                                                    :width-request 8}] 8 1))))
  (is (= ["[====]"] (scr/lines (paint [:progress {:value 1 :bar :brackets :halign :start
                                                  :width-request 6}] 6 1)))))
