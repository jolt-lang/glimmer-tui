(ns glimmer-tui.color-test
  "Colour props in, palette indices out."
  (:require [clojure.test :refer [deftest is testing]]
            [glimmer-tui.color :as color]))

(deftest names-and-indices
  (is (= 1 (color/index :red)))
  (is (= 12 (color/index :bright-blue)))
  (is (= 208 (color/index 208)))
  (is (= -1 (color/index nil)) "no colour means the terminal's own")
  (is (= -1 (color/index :default)))
  (is (= -1 (color/index :not-a-colour)))
  (is (= -1 (color/index 999)) "an index the palette does not have is not a colour"))

(deftest hex-and-rgb-fold-into-the-256-cube
  (is (= [255 102 68] (color/parse-hex "#f64")) "the short form doubles each digit")
  (is (= [255 100 50] (color/parse-hex "#ff6432")))
  (is (nil? (color/parse-hex "nope")))
  (testing "the extremes land on the cube's corners"
    (is (= 16 (color/rgb->ansi256 [0 0 0])))
    (is (= 231 (color/rgb->ansi256 [255 255 255]))))
  (testing "a grey goes to the grey ramp, which is ten times finer than the cube"
    (is (= 244 (color/rgb->ansi256 [128 128 128]))))
  (testing "a colour prop can be written any of the three ways"
    (is (= (color/index "#ff6432") (color/index [255 100 50])))))

(deftest downgrade-approximates-rather-than-failing
  (is (= 208 (color/downgrade 208 :ansi256)) "nothing to do on a 256-colour tty")
  (is (< (color/downgrade 208 :ansi16) 16) "folded into the sixteen")
  (is (< (color/downgrade 208 :ansi8) 8) "and into the eight")
  (is (= 3 (color/downgrade 3 :ansi8)) "a colour that already fits is left alone")
  (is (= -1 (color/downgrade 208 :mono)) "a monochrome terminal gets no colour")
  (is (= -1 (color/downgrade -1 :ansi256)) "the default stays the default"))

(deftest profiles-come-from-what-the-terminal-reports
  (is (= :ansi256 (color/profile 256)))
  (is (= :ansi16 (color/profile 16)))
  (is (= :ansi8 (color/profile 8)))
  (is (= :mono (color/profile 0))))
