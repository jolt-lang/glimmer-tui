(ns glimmer-tui.layout
  "Box layout over a widget snapshot. Pure functions on plain maps, so every rule
  below is unit-tested with no terminal in sight.

  Two passes, like every box model:

    measure  bottom-up, gives each node its natural OUTER size (what it wants,
             margins, padding and frame borders included)
    arrange  top-down, hands each node the box it actually gets and writes an
             inner :rect {:x :y :w :h} for the painter

  A box lays its children out along its :orientation, separated by :spacing.
  Leftover space on that axis goes to the children marked :hexpand (horizontal)
  or :vexpand (vertical), split evenly; if nobody expands, the leftover is left
  empty at the end. On the cross axis a child fills the box by default, which is
  what GTK does too, and :halign/:valign of :start, :center or :end opt out of
  that. When there is not enough room, children are served in order until it runs
  out — clipping the tail is the predictable choice for a box that cannot grow.

  Two container kinds do not follow that rule:

    :scroll   lays its child out at the child's full natural size, offset by the
              scroll position, and reports a :content size so the painter can
              draw a scrollbar and the input layer can clamp the offset. What
              does not fit is clipped by the renderer rather than lost.
    :overlay  is laid out against the SCREEN, not its parent, and takes no space
              where it is declared. That is what lets a dialog be written inside
              the component it belongs to and still float over everything."
  (:require [glimmer-tui.widget :as w]))

;; --- edges -------------------------------------------------------------------
(defn- box-values
  "CSS-ish shorthand: 2 is every edge, [1 2] is [vertical horizontal], and
  [1 2 3 4] is [top right bottom left]. Returns {:t :r :b :l}."
  [v]
  (cond
    (number? v) {:t v :r v :b v :l v}
    (and (sequential? v) (= 2 (count v))) (let [[vert horz] v]
                                            {:t vert :b vert :l horz :r horz})
    (and (sequential? v) (= 4 (count v))) (let [[t r b l] v] {:t t :r r :b b :l l})
    (and (sequential? v) (= 1 (count v))) (box-values (first v))
    :else {:t 0 :r 0 :b 0 :l 0}))

(defn edges
  "The four edge sizes for `kind` (:margin or :padding) in `props`, honouring
  both the shorthand (:margin 1, :margin [1 2]) and the per-edge props
  (:margin-start, :padding-top, …)."
  [props kind]
  (let [base (box-values (get props kind 0))
        named (fn [k fallback]
                (or (get props (keyword (str (name kind) "-" k))) fallback))]
    {:l (named "start" (:l base))
     :r (named "end" (:r base))
     :t (named "top" (:t base))
     :b (named "bottom" (:b base))}))

(defn- shrink
  "`rect` inset by the edge map `e`."
  [rect e]
  {:x (+ (:x rect) (:l e))
   :y (+ (:y rect) (:t e))
   :w (max 0 (- (:w rect) (:l e) (:r e)))
   :h (max 0 (- (:h rect) (:t e) (:b e)))})

(defn- horizontal? [n] (not= :vertical (:orientation (:props n) :vertical)))

(defn- leaf-size [n]
  (let [spec (w/spec-for (:tag n))
        m (:measure spec)
        {:keys [w h]} (if m (m (:props n) (:state n)) {:w 0 :h 0})]
    {:w w :h h}))

(defn- scrollbar?
  "Whether a scroll container reserves a column for its scrollbar."
  [n] (not (false? (:scrollbar (:props n)))))

(defn measure
  "Annotate every node in `n` with :natural, its outer size as a {:w :h}."
  [n]
  (let [kids (mapv measure (:children n))
        props (:props n)
        m (edges props :margin)
        p (edges props :padding)
        kind (w/container-kind (:tag n))
        inner
        (case kind
          :box (let [spacing (or (:spacing props) 0)
                     gaps (* spacing (max 0 (dec (count kids))))
                     ws (map #(:w (:natural %)) kids)
                     hs (map #(:h (:natural %)) kids)]
                 (if (horizontal? n)
                   {:w (+ (reduce + 0 ws) gaps) :h (reduce max 0 hs)}
                   {:w (reduce max 0 ws) :h (+ (reduce + 0 hs) gaps)}))
          ;; a frame spends a cell on each side for its border
          :frame (let [c (first kids)
                       b (:measure (w/spec-for (:tag n)))
                       border (if b (b props (:state n)) {:w 2 :h 2})]
                   {:w (+ (:w border) (:w (:natural c) 0))
                    :h (+ (:h border) (:h (:natural c) 0))})
          :window (let [c (first kids)]
                    {:w (:w (:natural c) 0) :h (:h (:natural c) 0)})
          ;; a scroll wants its child's size; when it is not given that much, the
          ;; shortfall becomes the scrollable range instead of being clipped away
          :scroll (let [c (first kids)]
                    {:w (+ (:w (:natural c) 0) (if (scrollbar? n) 1 0))
                     :h (:h (:natural c) 0)})
          ;; an overlay floats: it takes no room where it was declared
          :overlay {:w 0 :h 0}
          (leaf-size n))
        inner {:w (+ (:w inner) (:l p) (:r p))
               :h (+ (:h inner) (:t p) (:b p))}
        inner {:w (max (or (:width-request props) 0) (:w inner))
               :h (max (or (:height-request props) 0) (:h inner))}]
    (assoc n
           :children kids
           :natural {:w (+ (:w inner) (:l m) (:r m))
                     :h (+ (:h inner) (:t m) (:b m))})))

(defn- expands? [n axis]
  (if (= axis :h) (boolean (:hexpand (:props n))) (boolean (:vexpand (:props n)))))

(defn- align-offset
  "Where a child of size `size` sits inside `avail` cells, given its alignment."
  [align avail size]
  (case align
    :center (max 0 (quot (- avail size) 2))
    :end    (max 0 (- avail size))
    0))

(defn- cross-box
  "The cross-axis offset and size for a child: fill the parent unless the child
  asked for an alignment other than :fill."
  [child align-key avail]
  (let [align (get (:props child) align-key :fill)
        want (if (= align-key :halign)
               (:w (:natural child) 0)
               (:h (:natural child) 0))]
    (if (or (= align :fill) (expands? child (if (= align-key :halign) :h :v)))
      [0 avail]
      (let [size (min want avail)]
        [(align-offset align avail size) size]))))

(defn- distribute
  "How many cells each child gets on the main axis. Children are given their
  natural size in order while space lasts; any surplus is split evenly between
  the expanders, with the remainder handed to the earliest of them."
  [kids naturals avail spacing axis]
  (let [gaps (* spacing (max 0 (dec (count kids))))
        total (reduce + 0 naturals)
        room (- avail gaps)
        extra (- room total)
        expanders (vec (keep-indexed (fn [i c] (when (expands? c axis) i)) kids))]
    (cond
      ;; enough room, and somebody wants the rest
      (and (pos? extra) (seq expanders))
      ;; rank: expander index -> its position among the expanders, so the
      ;; remainder cells go to the earliest ones and the total lands exactly.
      (let [share (quot extra (count expanders))
            remainder (rem extra (count expanders))
            rank (into {} (map-indexed (fn [r i] [i r]) expanders))]
        (vec (map-indexed
               (fn [i nat]
                 (if-let [pos (rank i)]
                   (+ nat share (if (< pos remainder) 1 0))
                   nat))
               naturals)))

      ;; enough room and nobody expands: everyone gets exactly what they asked for
      (>= extra 0) (vec naturals)

      ;; not enough room: serve in order until it runs out
      :else
      (first (reduce (fn [[acc left] nat]
                       (let [got (max 0 (min nat left))]
                         [(conj acc got) (- left got)]))
                     [[] (max 0 room)]
                     naturals)))))

(declare arrange)

(defn- arrange-box [n rect screen]
  (let [props (:props n)
        kids (:children n)
        spacing (or (:spacing props) 0)
        h? (horizontal? n)
        axis (if h? :h :v)
        naturals (map (fn [c] (if h? (:w (:natural c) 0) (:h (:natural c) 0))) kids)
        sizes (distribute kids naturals (if h? (:w rect) (:h rect)) spacing axis)]
    (assoc n :children
           (first
             (reduce
               (fn [[acc pos] [child size]]
                 (let [[cross-off cross-size]
                       (cross-box child (if h? :valign :halign) (if h? (:h rect) (:w rect)))
                       child-rect (if h?
                                    {:x (+ (:x rect) pos) :y (+ (:y rect) cross-off)
                                     :w size :h cross-size}
                                    {:x (+ (:x rect) cross-off) :y (+ (:y rect) pos)
                                     :w cross-size :h size})]
                   [(conj acc (arrange child child-rect screen))
                    (+ pos size spacing)]))
               [[] 0]
               (map vector kids sizes))))))

(defn- arrange-scroll
  "The child is given its full natural size (never less than the viewport) and
  shifted by the scroll offset; :content records how big that is so the painter
  can size a scrollbar and the input layer can clamp the offset."
  [n rect screen]
  (let [child (first (:children n))
        state (:state n)
        bar? (and (scrollbar? n) child)
        view-w (max 0 (- (:w rect) (if bar? 1 0)))
        content-w (max view-w (:w (:natural child) 0))
        content-h (max (:h rect) (:h (:natural child) 0))
        off-x (max 0 (min (:x-offset state 0) (- content-w view-w)))
        off-y (max 0 (min (:y-offset state 0) (- content-h (:h rect))))]
    (assoc n
           :content {:w content-w :h content-h}
           :viewport {:w view-w :h (:h rect)}
           :offset {:x off-x :y off-y}
           :children (if child
                       [(arrange child
                                 {:x (- (:x rect) off-x) :y (- (:y rect) off-y)
                                  :w content-w :h content-h}
                                 screen)]
                       []))))

(defn- arrange-overlay
  "An overlay is placed against the screen, at the size its child asked for,
  anchored by :anchor and nudged by :offset-x / :offset-y."
  [n rect screen]
  (let [props (:props n)
        child (first (:children n))
        want-w (max (or (:width-request props) 0) (:w (:natural child) 0))
        want-h (max (or (:height-request props) 0) (:h (:natural child) 0))
        w (min want-w (:w screen))
        h (min want-h (:h screen))
        slack-x (max 0 (- (:w screen) w))
        slack-y (max 0 (- (:h screen) h))
        [ax ay] (case (:anchor props :center)
                  :center        [(quot slack-x 2) (quot slack-y 2)]
                  :top-left      [0 0]
                  :top-center    [(quot slack-x 2) 0]
                  :top-right     [slack-x 0]
                  :center-left   [0 (quot slack-y 2)]
                  :center-right  [slack-x (quot slack-y 2)]
                  :bottom-left   [0 slack-y]
                  :bottom-center [(quot slack-x 2) slack-y]
                  :bottom-right  [slack-x slack-y]
                  [(quot slack-x 2) (quot slack-y 2)])
        x (max 0 (min (- (:w screen) w) (+ (:x screen) ax (or (:offset-x props) 0))))
        y (max 0 (min (- (:h screen) h) (+ (:y screen) ay (or (:offset-y props) 0))))
        placed {:x x :y y :w w :h h}]
    (assoc n
           :rect placed
           :children (if child [(arrange child placed screen)] []))))

(defn arrange
  "Give `n` the outer box `rect` {:x :y :w :h}, writing its inner :rect (the box
  minus margins) and recursing into its children. `screen` is the whole terminal,
  which only overlays care about."
  [n rect screen]
  (let [kind (w/container-kind (:tag n))]
    (if (= :overlay kind)
      (arrange-overlay n rect screen)
      (let [inner (shrink rect (edges (:props n) :margin))
            n (assoc n :rect inner)
            content (shrink inner (edges (:props n) :padding))]
        (case kind
          :box (arrange-box n content screen)
          :scroll (arrange-scroll n content screen)
          ;; a frame's child lives inside the border, and then inside the padding
          :frame (assoc n :children
                        (if-let [c (first (:children n))]
                          (let [b (if (= :none (:border (:props n))) 0 1)]
                            [(arrange c {:x (+ (:x content) b) :y (+ (:y content) b)
                                         :w (max 0 (- (:w content) (* 2 b)))
                                         :h (max 0 (- (:h content) (* 2 b)))}
                                      screen)])
                          []))
          :window (assoc n :children
                         (if-let [c (first (:children n))]
                           [(arrange c content screen)]
                           []))
          n)))))

(defn layout
  "Measure and arrange `snapshot` into a `cols` x `rows` screen. The result is
  the same tree with :natural and :rect on every node."
  [snapshot cols rows]
  (let [screen {:x 0 :y 0 :w cols :h rows}]
    (-> snapshot
        measure
        (arrange screen screen))))
