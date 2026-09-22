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

(deftest paste-marker-recognises-the-wrapper-a-terminal-sends
  (testing "the two markers, as the codes that follow an ESC"
    (is (= :paste-start (k/paste-marker (codes-of "[200~"))))
    (is (= :paste-end (k/paste-marker (codes-of "[201~")))))
  (testing "a run that could still become either keeps the loop reading"
    (is (= :partial (k/paste-marker [])))
    (is (= :partial (k/paste-marker (codes-of "["))))
    (is (= :partial (k/paste-marker (codes-of "[20"))))
    (is (= :partial (k/paste-marker (codes-of "[200"))))
    (is (= :partial (k/paste-marker (codes-of "[201")))))
  (testing "and anything else is what it always was: alt-[ and some keys"
    (is (nil? (k/paste-marker (codes-of "b"))) "alt-b")
    (is (nil? (k/paste-marker (codes-of "[2~"))) "a real CSI key")
    (is (nil? (k/paste-marker (codes-of "[202~"))))
    (is (nil? (k/paste-marker (codes-of "[200~x"))) "past the end of a marker")))

(deftest a-paste-is-an-event-like-any-other
  (let [e (k/paste "two\nlines")]
    (is (= :paste (:type e)))
    (is (= "two\nlines" (:text e)))
    (is (k/match? e :paste))
    (is (k/match? e "paste"))
    (is (not (k/printable? e)) "so a field inserts it as text, not as keystrokes")
    (is (not (k/match? e "ctrl+c")) "and it can never be mistaken for a quit key")
    (is (= "" (:text (k/paste nil))))))
