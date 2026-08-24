(ns glimmer-tui.render
  "Paint a laid-out widget snapshot onto a screen.

  Parents paint before their children, so a box's background sits under its
  contents, and every node is clipped to its parent's rect. Clipping matters
  more in a terminal than it does in a GUI: a label that wants 40 columns inside
  a frame that has 20 will happily write over the frame's right border, and
  worse, over whatever is beside it. A subtree with no room left is skipped
  entirely.

  A widget is handed its OWN rect and a screen already clipped to the visible
  part of it, rather than the clipped rect itself. That distinction is what makes
  scrolling work: a row whose top half is above the viewport still draws its
  second line at the right place instead of restarting at the clip boundary.

  Overlays are hoisted out of the main pass and painted last, against the whole
  screen — a dialog is not clipped by whatever box it happens to be declared in.

  Pure with respect to the tree — the only thing it mutates is the screen it was
  handed, which in the tests is an in-memory buffer."
  (:require [glimmer-tui.screen :as scr]
            [glimmer-tui.widget :as w]))

(defn intersect
  "The overlap of two rects, or nil when they do not overlap."
  [a b]
  (when (and a b)
    (let [x (max (:x a) (:x b))
          y (max (:y a) (:y b))
          r (min (+ (:x a) (:w a)) (+ (:x b) (:w b)))
          bt (min (+ (:y a) (:h a)) (+ (:y b) (:h b)))]
      (when (and (> r x) (> bt y))
        {:x x :y y :w (- r x) :h (- bt y)}))))

(defn- paint-node! [screen n vis ctx]
  (when-let [spec (w/spec-for (:tag n))]
    (when-let [paint (:paint spec)]
      (paint (scr/clip screen vis) (:rect n) (:props n) n ctx))))

(defn- overlay? [n] (= :overlay (w/container-kind (:tag n))))

(defn- paint!
  "Paint `n` and its children, clipped to `clip`. Overlay subtrees are collected
  rather than painted, and returned so the caller can paint them on top."
  [screen n clip ctx]
  (if (overlay? n)
    [n]
    (if-let [vis (intersect (:rect n) clip)]
      (do (paint-node! screen n vis ctx)
          (vec (mapcat #(paint! screen % vis ctx) (:children n))))
      ;; an invisible subtree can still hold an overlay, which is anchored to the
      ;; screen and does not care that its declaration site scrolled out of view
      (vec (mapcat #(paint! screen % clip ctx) (:children n))))))

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
    ;; overlays paint last and against the whole screen, in declaration order, so
    ;; a later one sits on top of an earlier one — and an overlay declared inside
    ;; an overlay (a menu on a dialog) lands on top of both.
    (loop [pending (paint! screen tree full ctx)]
      (when (seq pending)
        (recur (vec (mapcat (fn [o]
                              (if-let [vis (intersect (:rect o) full)]
                                (do (paint-node! screen o vis ctx)
                                    (vec (mapcat #(paint! screen % vis ctx)
                                                 (:children o))))
                                []))
                            pending)))))
    (if-let [[cx cy] (cursor-position tree (:focus-id ctx))]
      (scr/cursor! screen cx cy true)
      (scr/cursor! screen 0 0 false))
    (scr/present! screen)
    nil))
