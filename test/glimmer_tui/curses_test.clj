(ns glimmer-tui.curses-test
  "The ncurses screen, checked at the only seam a headless suite can reach: the
  calls its :cursor! makes. The ncurses bindings are stubbed into a log, so no
  native call happens — what ncurses then does with the physical cursor needs a
  real tty and is out of reach here.

  Why it matters: start! sets leaveok(win, 1), which tells ncurses it need not
  move the physical cursor to the window's cursor position after a refresh. A
  :cursor! that only calls wmove would then be inert — the caret sits wherever
  the last cell was painted. So placing a cursor clears leaveok first, and so
  does hiding one: a terminal that still draws a hidden cursor (issue #20) shows
  it wherever it was left."
  (:require [clojure.test :refer [deftest is]]
            [glimmer-tui.curses :as curses]
            [glimmer-tui.ffi :as c]
            [glimmer-tui.screen :as scr]))

(defn- cursor-call-log
  "Build an ncurses screen, run `f` against it, and return every leaveok / wmove
  / curs-set call it made as [name & args], in order. The bindings are redefined
  for the whole body because a screen's closures resolve them at call time."
  [f]
  (let [log (atom [])
        note (fn [name] (fn [& args] (swap! log conj (into [name] args)) 0))]
    (with-redefs [c/has-colors (constantly false)
                  c/tigetnum    (constantly 0)
                  c/leaveok     (note :leaveok)
                  c/wmove       (note :wmove)
                  c/curs-set    (note :curs-set)]
      (f (curses/ncurses-screen ::win)))
    @log))

(deftest placing-the-cursor-clears-leaveok-so-ncurses-honours-the-move
  (let [win ::win]
    (is (= [[:leaveok win 0] [:wmove win 0 6] [:curs-set 1]]
           (cursor-call-log #(scr/cursor! % 6 0 true)))
        "leaveok is cleared before the move, so doupdate lands the cursor")))

(deftest a-hidden-cursor-is-placed-not-left-where-painting-ended
  (let [win ::win]
    (is (= [[:leaveok win 0] [:wmove win 2 5] [:curs-set 0]]
           (cursor-call-log #(scr/cursor! % 5 2 false)))
        "hidden at the place it was given")
    (is (= [[:leaveok win 0] [:wmove win 0 6] [:curs-set 1]
            [:leaveok win 0] [:wmove win 0 6] [:curs-set 0]]
           (cursor-call-log #(do (scr/cursor! % 6 0 true) (scr/cursor! % nil nil false))))
        "given no place, hidden where the last caret was")
    (is (= [[:leaveok win 0] [:wmove win 0 0] [:curs-set 0]]
           (cursor-call-log #(scr/cursor! % nil nil false)))
        "never placed, hidden at the origin")))
