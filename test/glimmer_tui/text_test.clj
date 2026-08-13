(ns glimmer-tui.text-test
  (:require [clojure.test :refer [deftest is testing]]
            [glimmer-tui.text :as text]))

(deftest width-counts-cells-not-characters
  (testing "ascii is one cell per character"
    (is (= 5 (text/width "hello"))))
  (testing "CJK takes two"
    (is (= 4 (text/width "日本")))
    (is (= 7 (text/width "日本 ok"))))
  (testing "combining marks take none"
    (is (= 1 (text/width "é"))))
  (testing "empty and nil"
    (is (= 0 (text/width "")))
    (is (= 0 (text/width nil)))))

(deftest truncate-never-splits-a-wide-glyph
  (is (= "hel" (text/truncate "hello" 3)))
  (is (= "hello" (text/truncate "hello" 10)))
  (testing "a wide glyph that would straddle the edge is dropped whole"
    (is (= "日" (text/truncate "日本" 3)))
    (is (= "日本" (text/truncate "日本" 4)))))

(deftest pad-fills-to-exactly-the-width
  (is (= "hi   " (text/pad "hi" 5)))
  (is (= 5 (text/width (text/pad "日本x" 5))))
  (testing "overlong text is truncated, not overflowed"
    (is (= "hel" (text/pad "hello" 3)))))

(deftest lines-and-block-width
  (is (= ["a" "bb"] (text/lines "a\nbb")))
  (is (= [""] (text/lines "")))
  (is (= 2 (text/block-width "a\nbb"))))
