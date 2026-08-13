(ns glimmer-tui.layout
  "Box layout over a widget snapshot. Pure functions on plain maps, so every rule
  below is unit-tested with no terminal in sight.

  Two passes, like every box model:

    measure  bottom-up, gives each node its natural OUTER size (what it wants,
             margins and frame borders included)
    arrange  top-down, hands each node the box it actually gets and writes an
             inner :rect {:x :y :w :h} for the painter

  A box lays its children out along its :orientation, separated by :spacing.
  Leftover space on that axis goes to the children marked :hexpand (horizontal)
  or :vexpand (vertical), split evenly; if nobody expands, the leftover is left
  empty at the end. On the cross axis a child fills the box by default, which is
  what GTK does too, and :halign/:valign of :start, :center or :end opt out of
  that. When there is not enough room, children are served in order until it runs
  out — a terminal cannot scroll what does not fit, so something has to give, and
  clipping the tail is the predictable choice."
  (:require [glimmer-tui.widget :as w]))

(defn- margins [props]
  (let [m (or (:margin props) 0)]
    {:l (or (:margin-start props) m)
     :r (or (:margin-end props) m)
     :t (or (:margin-top props) m)
     :b (or (:margin-bottom props) m)}))

(defn- horizontal? [n] (not= :vertical (:orientation (:props n) :vertical)))

(defn- leaf-size [n]
  (let [spec (w/spec-for (:tag n))
        m (:measure spec)
        {:keys [w h]} (if m (m (:props n) (:state n)) {:w 0 :h 0})]
    {:w w :h h}))

(defn measure
  "Annotate every node in `n` with :natural, its outer size as a {:w :h}."
  [n]
  (let [kids (mapv measure (:children n))
        props (:props n)
        {:keys [l r t b]} (margins props)
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
          :frame (let [c (first kids)]
                   {:w (+ 2 (:w (:natural c) 0)) :h (+ 2 (:h (:natural c) 0))})
          :window (let [c (first kids)]
                    {:w (:w (:natural c) 0) :h (:h (:natural c) 0)})
          (leaf-size n))
        inner {:w (max (or (:width-request props) 0) (:w inner))
               :h (max (or (:height-request props) 0) (:h inner))}]
    (assoc n
           :children kids
           :natural {:w (+ (:w inner) l r) :h (+ (:h inner) t b)})))

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

(defn- arrange-box [n rect]
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
                   [(conj acc (arrange child child-rect))
                    (+ pos size spacing)]))
               [[] 0]
               (map vector kids sizes))))))

(defn arrange
  "Give `n` the outer box `rect` {:x :y :w :h}, writing its inner :rect (the box
  minus margins) and recursing into its children."
  [n rect]
  (let [{:keys [l r t b]} (margins (:props n))
        inner {:x (+ (:x rect) l)
               :y (+ (:y rect) t)
               :w (max 0 (- (:w rect) l r))
               :h (max 0 (- (:h rect) t b))}
        n (assoc n :rect inner)]
    (case (w/container-kind (:tag n))
      :box (arrange-box n inner)
      ;; a frame's child lives inside the border
      :frame (assoc n :children
                    (if-let [c (first (:children n))]
                      [(arrange c {:x (inc (:x inner)) :y (inc (:y inner))
                                   :w (max 0 (- (:w inner) 2))
                                   :h (max 0 (- (:h inner) 2))})]
                      []))
      :window (assoc n :children
                     (if-let [c (first (:children n))]
                       [(arrange c inner)]
                       []))
      n)))

(defn layout
  "Measure and arrange `snapshot` into a `cols` x `rows` screen. The result is
  the same tree with :natural and :rect on every node."
  [snapshot cols rows]
  (-> snapshot
      measure
      (arrange {:x 0 :y 0 :w cols :h rows})))
