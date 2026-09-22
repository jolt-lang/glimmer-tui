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

(deftest clusters-hold-an-emoji-sequence-together
  (testing "a base and its combining mark are one cluster"
    (is (= ["é"] (text/clusters "é"))))
  (testing "a skin tone binds to the emoji before it"
    (is (= ["👍🏽"] (text/clusters "👍🏽")))
    (is (= 2 (text/width "👍🏽")) "one glyph, two cells — not two glyphs"))
  (testing "a ZWJ sequence is one cluster"
    (is (= 1 (count (text/clusters "👨‍💻")))))
  (testing "two regional indicators make one flag"
    (is (= 1 (count (text/clusters "🇯🇵"))))
    (is (= 2 (text/width "🇯🇵"))))
  (testing "a variation selector asks for emoji presentation, which is two cells"
    (is (= 2 (text/width "❤️")))
    (is (= 1 (text/width "❤"))))
  (testing "and a cluster is never cut in half"
    (is (= "" (text/truncate "👍🏽" 1)))
    (is (= "👍🏽" (text/truncate "👍🏽ok" 2)))))

(deftest drop-cells-cuts-from-the-left-on-a-cell-boundary
  (is (= "llo" (text/drop-cells "hello" 2)))
  (is (= "hello" (text/drop-cells "hello" 0)))
  (is (= "" (text/drop-cells "hello" 9)))
  (testing "a wide glyph straddling the cut becomes a space, so columns still line up"
    (is (= "本" (text/drop-cells "日本" 2)))
    (is (= " 本" (text/drop-cells "日本" 1)))
    (is (= 4 (text/width (text/drop-cells "日本x" 1))))
    (is (= " 本x" (text/drop-cells "日本x" 1)))))

(deftest the-ascii-fast-path-agrees-with-the-cluster-walk
  (testing "printable ASCII is measured by counting"
    (is (= 12 (text/width "hello, world")))
    (is (= 1 (text/width " ")))
    (is (= 1 (text/width "~")) "0x7e is the top of the fast path")
    (is (= ["a" "b" "c"] (text/clusters "abc"))))
  (testing "a control character still goes the long way round"
    (is (= 2 (text/width (str "a" (char 1) "b"))) "ctrl-a takes no cells")
    (is (= 1 (text/width (str (char 127) "x"))) "DEL takes none either")
    (is (= 2 (text/width (str "a\tb"))) "and neither does a tab"))
  (testing "cutting ASCII lands where cutting clusters would"
    (is (= "hello" (text/truncate "hello, world" 5)))
    (is (= "" (text/truncate "hello" 0)))
    (is (= "hello" (text/truncate "hello" 99)))
    (is (= ", world" (text/drop-cells "hello, world" 5)))
    (is (= "" (text/drop-cells "hello" 5)))
    (is (= "" (text/drop-cells "hello" 99)))
    (is (= "hel  " (text/pad "hel" 5)))))
