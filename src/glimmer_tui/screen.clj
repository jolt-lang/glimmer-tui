(ns glimmer-tui.screen
  "The surface widgets paint onto — one more seam, for the same reason glimmer
  has glimmer.backend.

  A screen is a plain map of functions:

    :size     (fn [] [cols rows])
    :clear!   (fn [])
    :put!     (fn [x y text style])   ; style: {:fg :bg :bold :underline :reverse :dim}
    :cursor!  (fn [x y visible?])
    :present! (fn [])                 ; flush the frame

  glimmer-tui.curses implements it over ncurses; `buffer-screen` below implements
  it as an in-memory grid. That second implementation is what makes this backend
  testable: the whole pipeline (reconcile, layout, paint) runs headless and the
  assertions read the rendered frame back as strings, with no terminal, no
  display and no ncurses call anywhere in the test suite."
  (:require [clojure.string :as str]
            [glimmer-tui.text :as text]))

;; The cell that a double-width glyph spills into. Kept in the grid so column
;; arithmetic stays honest, and filtered out when a row is read back as text.
(def ^:private wide-tail (char 0))

(defn size [scr] ((:size scr)))
(defn clear! [scr] ((:clear! scr)))
(defn put! [scr x y s style] ((:put! scr) x y s style))
(defn cursor! [scr x y visible?] ((:cursor! scr) x y visible?))
(defn present! [scr] ((:present! scr)))

(defn fill!
  "Paint `cells` spaces at (x, y) in `style` — the way a widget claims its
  background before drawing text over it."
  [scr x y cells style]
  (when (pos? cells)
    (put! scr x y (apply str (repeat cells " ")) style)))

;; --- in-memory screen --------------------------------------------------------
(defn buffer-screen
  "A screen backed by a `rows` x `cols` grid of characters held in an atom.
  Writes are clipped to the grid, so a widget painting out of bounds is a no-op
  rather than an exception, exactly as ncurses behaves.

  A wide glyph occupies its own cell and blanks the one after it, so reading a
  row back yields text of the right display width."
  [cols rows]
  (let [blank (vec (repeat rows (vec (repeat cols \space))))
        state (atom {:cells blank :styles {} :cursor nil :frames 0})]
    {:size     (fn [] [cols rows])
     :clear!   (fn [] (swap! state assoc :cells blank :styles {}) nil)
     :cursor!  (fn [x y visible?] (swap! state assoc :cursor (when visible? [x y])) nil)
     :present! (fn [] (swap! state update :frames inc) nil)
     :put!
     (fn [x y s style]
       (when (and (>= y 0) (< y rows))
         (swap! state
                (fn [st]
                  (loop [chars (seq s) cx x st st]
                    (if-let [c (first chars)]
                      (let [w (text/char-width (int c))]
                        (cond
                          ;; zero-width: nothing to place, and nothing to skip
                          (zero? w) (recur (next chars) cx st)
                          (or (< cx 0) (>= cx cols)) (recur (next chars) (+ cx w) st)
                          :else
                          (let [st (-> st
                                       (assoc-in [:cells y cx] c)
                                       (assoc-in [:styles [x y cx]] style))
                                ;; a double-width glyph owns the next cell too
                                st (if (and (= w 2) (< (inc cx) cols))
                                     (assoc-in st [:cells y (inc cx)] wide-tail)
                                     st)]
                            (recur (next chars) (+ cx w) st))))
                      st)))))
       nil)
     ::state   state}))

(defn lines
  "The buffer screen's contents as one string per row, right-trimmed. The
  placeholder cell that follows a wide glyph is dropped, so the string reads the
  way the terminal looks."
  [scr]
  (let [cells (:cells @(::state scr))]
    (mapv (fn [row]
            (-> (apply str (remove #(= % wide-tail) row))
                (str/replace #"\s+$" "")))
          cells)))

(defn text-at
  "`n` cells of row `y` starting at column `x`, as a string. Handy for asserting
  on one widget without depending on the rest of the frame."
  [scr x y n]
  (let [row (get (:cells @(::state scr)) y)]
    (apply str (remove #(= % wide-tail) (subvec row (max 0 x) (min (count row) (+ x n)))))))

(defn style-at
  "The style map a cell was painted with, or nil."
  [scr x y]
  (some (fn [[[_ sy cx] style]] (when (and (= sy y) (= cx x)) style))
        (:styles @(::state scr))))

(defn cursor
  "[x y] where the cursor was placed, or nil when hidden."
  [scr] (:cursor @(::state scr)))

(defn frames
  "How many times the screen has been presented — a repaint counter for tests."
  [scr] (:frames @(::state scr)))
