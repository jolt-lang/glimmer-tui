(ns glimmer-tui.render
  "Paint a laid-out widget snapshot onto a screen.

  Parents paint before their children, so a box's background sits under its
  contents, and every node is clipped to its parent's rect. Clipping matters
  more in a terminal than it does in a GUI: a label that wants 40 columns inside
  a frame that has 20 will happily write over the frame's right border, and
  worse, over whatever is beside it. A subtree with no room left is skipped
  entirely.

  Pure with respect to the tree — the only thing it mutates is the screen it was
  handed, which in the tests is an in-memory buffer."
  (:require [glimmer-tui.screen :as scr]
            [glimmer-tui.widget :as w]))

(defn- intersect
  "The overlap of two rects, or nil when they do not overlap."
  [a b]
  (when (and a b)
    (let [x (max (:x a) (:x b))
          y (max (:y a) (:y b))
          r (min (+ (:x a) (:w a)) (+ (:x b) (:w b)))
          bt (min (+ (:y a) (:h a)) (+ (:y b) (:h b)))]
      (when (and (> r x) (> bt y))
        {:x x :y y :w (- r x) :h (- bt y)}))))

(defn- paint-node! [screen n clip ctx]
  (when-let [spec (w/spec-for (:tag n))]
    (when-let [paint (:paint spec)]
      (paint screen clip (:props n) n ctx))))

(defn- paint! [screen n clip ctx]
  (when-let [rect (intersect (:rect n) clip)]
    (paint-node! screen n rect ctx)
    (doseq [child (:children n)]
      (paint! screen child rect ctx))))

(defn- find-node [n id]
  (first (filter #(= id (:id %)) (w/walk n))))

(defn cursor-position
  "Where the terminal cursor belongs: the focused widget decides, via its spec's
  :cursor fn. nil means hide it, which is what everything except a text entry
  wants — a blinking cursor parked on a button reads as a glitch."
  [tree focus-id]
  (when focus-id
    (when-let [n (find-node tree focus-id)]
      (when-let [f (:cursor (w/spec-for (:tag n)))]
        (f (:rect n) (:props n) (:state n))))))

(defn render!
  "Clear `screen`, paint the laid-out `tree` onto it, place the cursor and
  present the frame. `ctx` carries {:focus-id id} so widgets can draw themselves
  focused."
  [screen tree ctx]
  (let [[cols rows] (scr/size screen)
        full {:x 0 :y 0 :w cols :h rows}]
    (scr/clear! screen)
    (paint! screen tree full ctx)
    (if-let [[cx cy] (cursor-position tree (:focus-id ctx))]
      (scr/cursor! screen cx cy true)
      (scr/cursor! screen 0 0 false))
    (scr/present! screen)
    nil))
