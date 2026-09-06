(ns glimmer-tui.widget
  "The widget registry and the node tree.

  A widget is a jolt atom holding

    {:id 7 :tag :button :props {...} :children [widget...] :state {...}}

  Creating one and patching its props are therefore pure data operations: no
  terminal is touched until the event loop repaints. That is what lets the whole
  reconcile/layout/paint pipeline run headless in the tests.

  :props is what the component rendered. :state is what the widget owns and the
  component does not: an entry's edit buffer, a list's cursor, a scroll offset.
  Reagent-style, a prop change only overwrites that state when the incoming prop
  actually differs from what the widget is holding, so typing is not clobbered by
  the re-render that typing itself triggers. A spec says how through :init-state
  and :sync-state.

  The widgets themselves live in glimmer-tui.widgets and the namespaces under
  it; this namespace knows only the shape of a spec, which is:

    :container   :box | :frame | :window | :scroll | :overlay | :none
    :measure     (fn [props state] {:w :h})    natural inner size of a leaf
    :paint       (fn [screen rect props node ctx])
                   `screen` is already clipped to the visible part of `rect`,
                   and `rect` is the node's OWN rect — paint at its coordinates
                   and let the clip discard what is off-screen
    :focusable?  (fn [node]) -> boolean        the whole node, children included
    :activate    (fn [widget])                 Enter / Space / click
    :key         (fn [widget event]) -> handled?   see glimmer-tui.keys
    :cursor      (fn [rect props state]) -> [x y]  where to park the terminal cursor
    :init-state  (fn [props]) -> state         state a fresh widget starts with
    :sync-state  (fn [props state]) -> state   state after a re-render
    :bindings    (fn [props]) -> [[action spec] …]  what keys this widget
                                               answers to, for a help bar to render
    :derive-state (fn [props state ctx]) -> state   state that depends on the
                                               loop rather than on props — what
                                               is focused, what keys are live.
                                               Applied before every layout."
  (:require [glimmer-tui.text :as text]))

;; --- widget nodes ------------------------------------------------------------
(defonce ^:private next-id (atom 0))

;; Every mutation bumps this. The event loop repaints when it changes, so a
;; reconcile that touches nothing costs nothing.
(defonce dirty (atom 0))

(defn touch!
  "Mark the widget tree as needing a repaint."
  [] (swap! dirty inc) nil)

(defn node
  "A fresh widget node for `tag` with `props`."
  [tag props]
  (atom {:id (swap! next-id inc) :tag tag :props props :children [] :state {}}))

(defn snapshot
  "Deref a widget subtree into a plain map — {:id :tag :props :state :children}.
  Layout and painting work on the snapshot, so they are pure functions over data
  and can be unit-tested with no widgets, no screen and no terminal."
  [w]
  (let [{:keys [id tag props state children]} @w]
    {:id id :tag tag :props props :state state
     :children (mapv snapshot children)}))

;; --- the registry ------------------------------------------------------------
(defonce specs (atom {}))

(defn register-widget!
  "Register a widget spec under hiccup `tag`. Lets a consumer add tags without
  editing this namespace, the way glimmer-gtk's register-widget! does."
  [tag spec] (swap! specs assoc tag spec) nil)

;; :hbox / :vbox are both boxes; the tag only picks the orientation.
(def ^:private aliases {:hbox :box :vbox :box})
(def ^:private tag-orientation {:hbox :horizontal :vbox :vertical})

(defn normalize-tag [tag] (get aliases tag tag))

(defn with-orientation
  "Inject the orientation implied by an :hbox/:vbox tag, unless props set it."
  [tag props]
  (if-let [o (tag-orientation tag)]
    (if (contains? props :orientation) props (assoc props :orientation o))
    props))

(defn spec-for [tag] (@specs (normalize-tag tag)))

(defn container-kind
  "How a tag holds children: :box, :frame, :window, :scroll, :overlay, or :none
  for a leaf."
  [tag] (:container (spec-for tag) :none))

(defn focusable?
  "Can this node take keyboard focus? Disabled widgets (:sensitive false) never
  do, so tabbing skips them."
  [n]
  (let [f (:focusable? (spec-for (:tag n)))]
    (boolean (and f (not (false? (:sensitive (:props n)))) (f n)))))

(defn derive-state!
  "Give every widget in the live tree rooted at `wid` a chance to fold
  loop-level context (`ctx`) into its own state, before the tree is laid out.
  This is how a help bar knows what is focused: the alternative is for the
  renderer to special-case a widget tag, and the registry exists precisely so it
  does not have to."
  [wid ctx]
  (when wid
    (when-let [f (:derive-state (spec-for (:tag @wid)))]
      (let [{:keys [props state]} @wid
            next-state (f props state ctx)]
        (when (not= next-state state)
          (swap! wid assoc :state next-state))))
    (doseq [c (:children @wid)] (derive-state! c ctx)))
  nil)

(defn bindings-of
  "The key bindings `n` answers to, as {action spec}, or nil. A help bar reads
  these off the live tree rather than being told them twice."
  [n]
  (when-let [f (:bindings (spec-for (:tag n)))]
    (f (:props n))))

;; --- styles ------------------------------------------------------------------
(defn style
  "The style map for a node: colours and attributes from props, plus the
  reverse-video marker a focused widget paints itself with."
  ([props] (style props false))
  ([props focused?]
   (cond-> {}
     (:color props)     (assoc :fg (:color props))
     (:bg props)        (assoc :bg (:bg props))
     (:bold props)      (assoc :bold true)
     (:underline props) (assoc :underline true)
     (:dim props)       (assoc :dim true)
     (:blink props)     (assoc :blink true)
     (false? (:sensitive props)) (assoc :dim true)
     (or focused? (:reverse props)) (assoc :reverse true))))

(defn text-align
  "Place `s` inside `cells` columns according to `align` (:start, :center, :end,
  or :fill which is the same as :start). Always returns exactly `cells` columns,
  so a repaint overwrites what was underneath."
  [s cells align]
  (let [t (text/truncate (str s) cells)
        slack (max 0 (- cells (text/width t)))
        left (case align
               :center (quot slack 2)
               :end slack
               0)]
    (str (apply str (repeat left " ")) t
         (apply str (repeat (- slack left) " ")))))

;; --- backend operations ------------------------------------------------------
(defn create!
  "Construct a widget for `tag` with `props`. Pure data: nothing is drawn until
  the event loop's next repaint."
  [tag props]
  (let [props (with-orientation tag props)
        tag (normalize-tag tag)
        w (node tag props)]
    (when-let [init (:init-state (spec-for tag))]
      (swap! w assoc :state (init props)))
    (touch!)
    w))

(defn apply-props!
  "Re-apply props to an existing widget (the re-render path).

  A widget's own state is only overwritten when the prop it mirrors actually
  differs from what the widget is holding — see each spec's :sync-state. Without
  that check, the :on-change -> swap! -> re-render cycle would write an entry's
  buffer back over itself and park the caret at the end on every keystroke: the
  terminal equivalent of the suppressing setters in glimmer-gtk."
  [tag w props]
  (let [props (with-orientation tag props)
        tag (normalize-tag tag)]
    (swap! w assoc :props props)
    (when-let [sync (:sync-state (spec-for tag))]
      (swap! w update :state #(sync props %)))
    (touch!)
    nil))

(defn- index-of [children child]
  (first (keep-indexed (fn [i c] (when (= (:id @c) (:id @child)) i)) children)))

(defn append-child! [_parent-tag parent child]
  (swap! parent update :children conj child) (touch!) nil)

(defn remove-child! [_parent-tag parent child]
  (swap! parent update :children
         (fn [cs] (vec (remove #(= (:id @%) (:id @child)) cs))))
  (touch!) nil)

(defn replace-child! [_parent-tag parent old-child new-child]
  (swap! parent update :children
         (fn [cs] (if-let [i (index-of cs old-child)]
                    (assoc cs i new-child)
                    (conj cs new-child))))
  (touch!) nil)

(defn reorder-child!
  "Move `child` to sit immediately after `sibling` (nil = first). This is what
  keyed reconciliation calls when a list is reordered."
  [_parent-tag parent child sibling]
  (swap! parent update :children
         (fn [cs]
           (let [without (vec (remove #(= (:id @%) (:id @child)) cs))
                 pos (if sibling
                       (if-let [i (index-of without sibling)] (inc i) (count without))
                       0)]
             (vec (concat (subvec without 0 pos) [child] (subvec without pos))))))
  (touch!) nil)

;; --- tree queries (used by focus, scrolling and mouse handling) --------------
(defn walk
  "Depth-first sequence of nodes in a snapshot, parents before children."
  [n]
  (cons n (mapcat walk (:children n))))

(defn path-to
  "The chain of nodes from the root of `snapshot` down to the node with `id`,
  root first, or nil. Used to find the scroll containers a focused widget lives
  in, and to bubble a key outwards from the widget that declined it."
  [snapshot id]
  (if (= id (:id snapshot))
    [snapshot]
    (some (fn [c] (when-let [p (path-to c id)] (into [snapshot] p)))
          (:children snapshot))))

(defn focus-ring
  "Ids of the focusable nodes in `snapshot`, in tab order (tree order)."
  [snapshot]
  (vec (keep (fn [n] (when (focusable? n) (:id n))) (walk snapshot))))

(defn inside?
  "Whether the cell (x, y) is inside `n`'s laid-out rect."
  [n x y]
  (let [rect (:rect n)]
    (and rect
         (>= x (:x rect)) (< x (+ (:x rect) (:w rect)))
         (>= y (:y rect)) (< y (+ (:y rect) (:h rect))))))

(defn hit
  "The innermost focusable node in a laid-out `snapshot` whose rect contains the
  cell (x, y), or nil. Later siblings win, matching paint order."
  [snapshot x y]
  (last (filter (fn [n] (and (inside? n x y) (focusable? n))) (walk snapshot))))

(defn hit-kind
  "The innermost node of container kind `kind` covering the cell (x, y), or nil.
  This is how a mouse wheel finds the scroll container under the pointer."
  [snapshot kind x y]
  (last (filter (fn [n] (and (inside? n x y) (= kind (container-kind (:tag n)))))
                (walk snapshot))))

(defn find-widget
  "The live widget atom with `id` inside the tree rooted at `w`, or nil. The
  snapshot is what gets laid out and hit-tested, but activating a widget has to
  reach the atom the handlers live on."
  [w id]
  (when w
    (if (= id (:id @w))
      w
      (some #(find-widget % id) (:children @w)))))
