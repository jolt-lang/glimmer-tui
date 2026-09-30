(ns glimmer-tui.keys-test
  "Key codes in, named events out, and matching by name."
  (:require [clojure.test :refer [deftest is testing]]
            [glimmer-tui.keys :as k]))

(deftest decode-names-what-ncurses-numbers
  (is (= :page-up (:type (k/decode 339))))
  (is (= :backspace (:type (k/decode 263))))
  (is (= :backspace (:type (k/decode 127))) "the DEL most terminals actually send")
  (is (= :enter (:type (k/decode 10))))
  (is (= :back-tab (:type (k/decode 353))))
  (testing "a C0 control is a ctrl chord"
    (is (= {:type :ctrl :ch \u :code 21} (k/decode 21)))
    (is (= {:type :ctrl :ch \c :code 3} (k/decode 3))))
  (testing "printable characters carry the character"
    (is (= {:type :char :ch \j :code 106} (k/decode 106)))
    (is (= \space (:ch (k/decode 32)))))
  (testing "function keys"
    (is (= :f1 (:type (k/decode 265))))
    (is (= :f12 (:type (k/decode 276))))))

(deftest match-by-name
  (is (k/match? (k/decode 21) "ctrl+u"))
  (is (k/match? (k/decode 21) "CTRL+U") "the modifier is not case sensitive")
  (is (k/match? (k/decode 106) "j"))
  (is (not (k/match? (k/decode 106) "J")) "but a bare character is")
  (is (k/match? (k/decode 339) :page-up) "a keyword names the event type")
  (is (k/match? (k/decode 339) "pgup"))
  (is (not (k/match? (k/decode 339) "pgdown")) "…and pgdn is not pgdown's key")
  (is (k/match? (k/decode 353) "shift+tab"))
  (is (k/match? (k/decode 3) 3) "a raw code still matches, for code that has one")
  (testing "a collection matches if any of it does"
    (is (k/match? (k/decode 360) ["end" "G"]))
    (is (k/match? (k/decode 71) ["end" "G"]))
    (is (not (k/match? (k/decode 106) ["end" "G"]))))
  (testing "alt chords, which the loop assembles from ESC and the next key"
    (is (k/match? {:type :alt :ch \b :base-type :char} "alt+b"))
    (is (not (k/match? {:type :alt :ch \b :base-type :char} "ctrl+b")))))

(deftest matches-any-returns-the-action
  (let [bindings [[:up ["up" "k"]] [:down ["down" "j"]]]]
    (is (= :up (k/matches-any? (k/decode 259) bindings)))
    (is (= :down (k/matches-any? (k/decode 106) bindings)))
    (is (nil? (k/matches-any? (k/decode 113) bindings)))))

(deftest merge-bindings-keeps-declaration-order
  (let [defaults [[:up ["up"]] [:down ["down"]]]]
    (is (= [[:up ["up"]] [:down ["down"]]] (k/merge-bindings defaults nil)))
    (is (= [[:up ["w"]] [:down ["down"]]] (k/merge-bindings defaults {:up ["w"]}))
        "an override replaces the spec in place")
    (is (= [[:up ["up"]] [:down ["down"]] [:quit ["q"]]]
           (k/merge-bindings defaults {:quit ["q"]}))
        "an unknown action is appended")))

(deftest describe-labels-a-binding-for-a-help-bar
  (is (= "ctrl+u" (k/describe "ctrl+u")))
  (is (= "↑" (k/describe :up)))
  (is (= "pgup/ctrl+b" (k/describe ["pgup" "ctrl+b"])))
  (is (= "g" (k/describe "g"))))

;; --- bracketed paste ---------------------------------------------------------
(defn- codes-of
  "The key codes a terminal sends for `s` — what the event loop reads one at a
  time after an ESC."
  [s] (mapv int s))

(deftest escape-run-recognises-the-paste-wrapper-a-terminal-sends
  (testing "the two markers, as the codes that follow an ESC"
    (is (= :paste-start (k/escape-run (codes-of "[200~"))))
    (is (= :paste-end (k/escape-run (codes-of "[201~")))))
  (testing "a run that could still become either keeps the loop reading"
    (is (= :partial (k/escape-run [])))
    (is (= :partial (k/escape-run (codes-of "["))))
    (is (= :partial (k/escape-run (codes-of "[20"))))
    (is (= :partial (k/escape-run (codes-of "[200"))))
    (is (= :partial (k/escape-run (codes-of "[201")))))
  (testing "and anything else is what it always was: alt-[ and some keys"
    (is (nil? (k/escape-run (codes-of "b"))) "alt-b")
    (is (nil? (k/escape-run (codes-of "[2~"))) "a real CSI key")
    (is (nil? (k/escape-run (codes-of "[202~"))))
    (is (nil? (k/escape-run (codes-of "[200~x"))) "past the end of a marker")))

(deftest a-paste-is-an-event-like-any-other
  (let [e (k/paste "two\nlines")]
    (is (= :paste (:type e)))
    (is (= "two\nlines" (:text e)))
    (is (k/match? e :paste))
    (is (k/match? e "paste"))
    (is (not (k/printable? e)) "so a field inserts it as text, not as keystrokes")
    (is (not (k/match? e "ctrl+c")) "and it can never be mistaken for a quit key")
    (is (= "" (:text (k/paste nil))))
    (is (nil? (:cut? e)) "a paste that ended with its marker arrived whole")
    (is (= {:type :paste :text "x" :cut? true} (k/paste "x" true))
        "one cut short by the paste timeout says so, so an app knows the keys
         that follow are the rest of it")))

;; --- UTF-8 -------------------------------------------------------------------
;; wgetch hands the loop one byte at a time, but a terminal types anything past
;; ASCII as a multi-byte UTF-8 sequence: 한 is ED 95 9C. One byte one character
;; is mojibake — í plus two box-drawing ghosts — so the sequence is assembled
;; back into the character it is, which is what wget_wch would have returned.
(deftest a-lead-byte-says-how-many-continuations-follow
  (is (= 1 (k/utf8-length 0xC2)) "two-byte form: Latin-1 supplement and up")
  (is (= 2 (k/utf8-length 0xED)))
  (is (= 3 (k/utf8-length 0xF0)) "four-byte form: astral plane, emoji")
  (is (nil? (k/utf8-length 0x41)) "ASCII is not a lead byte")
  (is (nil? (k/utf8-length 0x80)) "nor is a continuation byte")
  (is (nil? (k/utf8-length 0xC0)) "nor C0/C1, the overlong forms no encoder emits"))

(deftest a-lead-and-its-continuations-are-one-character
  (is (= \한 (k/utf8-char 0xED [0x95 0x9C])))
  (is (= \é (k/utf8-char 0xC3 [0xA9])))
  (is (= (char 0x1F600) (k/utf8-char 0xF0 [0x9F 0x98 0x80])))
  (testing "and bytes that do not form a sequence are nobody's"
    (is (nil? (k/utf8-char 0xED [0x95])) "too few continuations")
    (is (nil? (k/utf8-char 0xED [0x95 0x9C 0x9C])) "too many")
    (is (nil? (k/utf8-char 0xED [0x41 0x9C])) "a byte that is not a continuation")))

(deftest utf8-text-decodes-a-run-of-bytes
  (is (= "é한" (k/utf8-text [0xC3 0xA9 0xED 0x95 0x9C])))
  (is (= "a한b" (k/utf8-text [0x61 0xED 0x95 0x9C 0x62])))
  (is (= (str (char 0x1F600)) (k/utf8-text [0xF0 0x9F 0x98 0x80])))
  (is (= "ab" (k/utf8-text [0x61 0x62])) "plain ASCII is untouched")
  (testing "a malformed sequence keeps its bytes as what they are"
    (is (= "íab" (k/utf8-text [0xED 0x61 0x62]))
        "the lead falls back to its Latin-1 meaning and the rest types on")
    (is (= (str (char 0xED) (char 0x95)) (k/utf8-text [0xED 0x95]))
        "a truncated sequence at the end")))

;; --- SGR mouse ---------------------------------------------------------------
;; With SGR mouse reporting on (glimmer-tui.curses), a terminal sends every mouse
;; event as ESC [ < b ; x ; y M|m. ncurses' own decoding is bypassed on purpose:
;; its version-1 ABI (stock macOS) has no button 5, so wheel-down is unreachable
;; through getmouse, and the button shift differs between version 1 and 2. The
;; SGR form is version-independent and carries both wheel directions.
(deftest escape-run-tells-a-paste-marker-from-a-mouse-report
  (testing "the paste markers are unchanged"
    (is (= :paste-start (k/escape-run (codes-of "[200~"))))
    (is (= :paste-end (k/escape-run (codes-of "[201~")))))
  (testing "an SGR mouse report is complete once its M or m arrives"
    (is (= :mouse (k/escape-run (codes-of "[<0;3;7M"))))
    (is (= :mouse (k/escape-run (codes-of "[<0;3;7m"))))
    (is (= :mouse (k/escape-run (codes-of "[<64;5;10M")))))
  (testing "a report still arriving keeps the loop reading"
    (is (= :partial (k/escape-run (codes-of "["))) "could still be [<")
    (is (= :partial (k/escape-run (codes-of "[<"))))
    (is (= :partial (k/escape-run (codes-of "[<64;5;10")))))
  (testing "and anything else is what it always was"
    (is (nil? (k/escape-run (codes-of "b"))) "alt-b")
    (is (nil? (k/escape-run (codes-of "[2~"))) "a real CSI key")))

(deftest sgr-mouse-reads-the-button-position-and-direction
  (testing "the wheel: button 64 is up, 65 is down, and both are reachable"
    (is (= {:type :mouse :button nil :wheel :up :x 4 :y 9 :action :press :motion false}
           (k/sgr-mouse (codes-of "[<64;5;10M"))))
    (is (= {:type :mouse :button nil :wheel :down :x 4 :y 9 :action :press :motion false}
           (k/sgr-mouse (codes-of "[<65;5;10M")))))
  (testing "button 0 is the left button; press and release are distinct"
    (is (= {:type :mouse :button 0 :wheel nil :x 2 :y 6 :action :press :motion false}
           (k/sgr-mouse (codes-of "[<0;3;7M"))))
    (is (= :release (:action (k/sgr-mouse (codes-of "[<0;3;7m"))))))
  (testing "the motion bit (32) survives: a drag is a drag, not another press"
    (is (= true (:motion (k/sgr-mouse (codes-of "[<32;3;7M"))))
        "32 alone is a motion with the left button held")
    (is (= true (:motion (k/sgr-mouse (codes-of "[<35;3;7m"))))
        "35 = motion | release-report | left, a drag that ends off the widget")
    (is (= false (:motion (k/sgr-mouse (codes-of "[<0;3;7M"))))))
  (testing "a horizontal wheel (66, 67) is not mistaken for up and down"
    (is (= :left (:wheel (k/sgr-mouse (codes-of "[<66;5;10M")))))
    (is (= :right (:wheel (k/sgr-mouse (codes-of "[<67;5;10M"))))))
  (testing "modifier bits do not change which button or wheel it is"
    (is (= :down (:wheel (k/sgr-mouse (codes-of "[<81;5;10M")))) "ctrl+wheel-down")
    (is (= 0 (:button (k/sgr-mouse (codes-of "[<4;5;10M")))) "shift+left"))
  (testing "extra buttons (128 and up) are neither a wheel nor a left click"
    (let [e (k/sgr-mouse (codes-of "[<128;5;10M"))]
      (is (nil? (:wheel e)))
      (is (not= 0 (:button e)))))
  (testing "positions are 1-based on the wire and 0-based here"
    (let [e (k/sgr-mouse (codes-of "[<0;1;1M"))]
      (is (= [0 0] [(:x e) (:y e)]))))
  (testing "a run that is not a report parses to nil"
    (is (nil? (k/sgr-mouse (codes-of "[<nopeM"))))
    (is (nil? (k/sgr-mouse (codes-of "[200~"))))))
