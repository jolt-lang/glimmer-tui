(ns glimmer-tui.curses-test
  "The ncurses screen, checked at the only seam a headless suite can reach: the
  calls its :cursor! makes. The ncurses bindings are stubbed into a log, so no
  native call happens — what ncurses then does with the physical cursor needs a
  real tty and is out of reach here.

  Why it matters: start! sets leaveok(win, 1), which tells ncurses it need not
  move the physical cursor to the window's cursor position after a refresh. A
  :cursor! that only calls wmove would then be inert — the caret sits wherever
  the last cell was painted. So placing a cursor must clear leaveok first, and
  hiding one may put it back."
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

(deftest hiding-the-cursor-restores-leaveok
  (let [win ::win]
    (is (= [[:curs-set 0] [:leaveok win 1]]
           (cursor-call-log #(scr/cursor! % 0 0 false)))
        "a UI with no caret keeps the saving")))
