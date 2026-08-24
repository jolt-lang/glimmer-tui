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
  display and no ncurses call anywhere in the test suite.

  `clip` wraps any screen in a rectangle. That is how a widget can be handed its
  own full rect to paint into while only the visible part of it reaches the
  terminal — which is what makes a partially scrolled row draw correctly instead
  of being redrawn from its own left edge at the clip boundary."
  (:require [clojure.string :as str]
            [glimmer-tui.text :as text]))

;; The cell that a double-width glyph spills into. Kept in the grid so column
;; arithmetic stays honest, and filtered out when a row is read back as text.
(def ^:private wide-tail ::tail)

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

;; --- clipping ----------------------------------------------------------------
(defn clip
  "A view of `scr` that only lets writes inside `rect` through. Writes that
  straddle an edge are cut on a cell boundary; a wide glyph cut in half becomes a
  space, so the columns still line up. Clips compose, so nesting one inside
  another narrows the window rather than widening it."
  [scr {:keys [x y w h] :as rect}]
  (if (nil? rect)
    scr
    (assoc scr
           :put!
           (fn [px py s style]
             (when (and (>= py y) (< py (+ y h)) (pos? w))
               (let [hidden (- x px)
                     s (if (pos? hidden) (text/drop-cells s hidden) s)
                     px (max px x)
                     room (- (+ x w) px)]
                 (when (pos? room)
                   (put! scr px py (text/truncate s room) style)))))
           :cursor!
           (fn [cx cy visible?]
             (cursor! scr cx cy (and visible?
                                     (>= cx x) (< cx (+ x w))
                                     (>= cy y) (< cy (+ y h))))))))

;; --- in-memory screen --------------------------------------------------------
(defn buffer-screen
  "A screen backed by a `rows` x `cols` grid of grapheme clusters held in an
  atom. Writes are clipped to the grid, so a widget painting out of bounds is a
  no-op rather than an exception, exactly as ncurses behaves.

  A wide cluster occupies its own cell and blanks the one after it, so reading a
  row back yields text of the right display width."
  [cols rows]
  (let [blank (vec (repeat rows (vec (repeat cols " "))))
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
                  (loop [cs (text/clusters s) cx x st st]
                    (if-let [c (first cs)]
                      (let [w (text/cluster-width c)]
                        (cond
                          ;; zero-width: nothing to place, and nothing to skip
                          (zero? w) (recur (next cs) cx st)
                          (or (< cx 0) (>= cx cols)) (recur (next cs) (+ cx w) st)
                          :else
                          (let [st (-> st
                                       (assoc-in [:cells y cx] c)
                                       (assoc-in [:styles [x y cx]] style))
                                ;; a double-width cluster owns the next cell too
                                st (if (and (= w 2) (< (inc cx) cols))
                                     (assoc-in st [:cells y (inc cx)] wide-tail)
                                     st)]
                            (recur (next cs) (+ cx w) st))))
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
