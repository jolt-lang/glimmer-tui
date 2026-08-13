(ns glimmer-tui.widget
  "Hiccup tags -> terminal widgets. A data-driven registry maps a tag to how it
  measures, how it paints, whether it takes focus and what a key or a click does
  to it — the same shape of registry glimmer-gtk keeps, except that nothing here
  talks to a C toolkit.

  A widget is a jolt atom holding

    {:id 7 :tag :button :props {...} :children [widget...] :state {...}}

  Creating one and patching its props are therefore pure data operations: no
  terminal is touched until the event loop repaints. That is what lets the whole
  reconcile/layout/paint pipeline run headless in the tests.

  :props is what the component rendered. :state is what the widget owns and the
  component does not: an entry's edit buffer and cursor. Reagent-style, a prop
  change only overwrites that buffer when the incoming :text actually differs
  from what the widget is holding, so typing is not clobbered by the re-render
  that typing itself triggers."
  (:require [glimmer-tui.screen :as scr]
            [glimmer-tui.text :as text]))

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
;; A spec is
;;   :container  :box | :frame | :window | :none   how it holds children
;;   :measure    (fn [props state] {:w :h})        natural inner size of a leaf
;;   :paint      (fn [screen rect props node ctx]) draw self (not children)
;;   :focusable? (fn [props] boolean)
;;   :activate   (fn [widget-atom] ...)            Enter / Space / click
;;   :key        (fn [widget-atom keycode] handled?)  text input, arrows
;;   :cursor     (fn [rect props state] [x y])     where to park the terminal cursor
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
  "How a tag holds children: :box, :frame, :window, or :none for a leaf."
  [tag] (:container (spec-for tag) :none))

(defn focusable?
  "Can this node take keyboard focus? Disabled widgets (:sensitive false) never
  do, so tabbing skips them."
  [n]
  (let [s (spec-for (:tag n))
        f (:focusable? s)]
    (boolean (and f (not (false? (:sensitive (:props n)))) (f (:props n))))))

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
     (false? (:sensitive props)) (assoc :dim true)
     (or focused? (:reverse props)) (assoc :reverse true))))

;; --- built-in widgets --------------------------------------------------------
(defn- label-text [props]
  (str (or (:label props) (:text props) "")))

(defn- label-spec []
  {:container :none
   :measure (fn [props _] (let [t (label-text props)]
                            {:w (text/block-width t) :h (count (text/lines t))}))
   :paint   (fn [screen rect props n _]
              (let [st (style props)]
                (doseq [[i line] (map-indexed vector (text/lines (label-text props)))
                        :while (< i (:h rect))]
                  (scr/put! screen (:x rect) (+ (:y rect) i)
                            (text/pad line (:w rect)) st))))})

;; A button is its label inside brackets, which is the convention every terminal
;; UI uses and needs no colour to read. Focus shows as reverse video.
(defn- button-label [props] (str "[ " (or (:label props) "") " ]"))

(defn- button-spec []
  {:container :none
   :measure (fn [props _] {:w (text/width (button-label props)) :h 1})
   :paint   (fn [screen rect props n ctx]
              (scr/put! screen (:x rect) (:y rect)
                        (text/pad (button-label props) (:w rect))
                        (style props (= (:id n) (:focus-id ctx)))))
   :focusable? (fn [_] true)
   :activate (fn [w] (when-let [f (:on-click (:props @w))] (f)))})

(defn- checkbutton-spec []
  {:container :none
   :measure (fn [props _]
              {:w (+ 4 (text/width (str (:label props)))) :h 1})
   :paint (fn [screen rect props n ctx]
            (let [s (str "[" (if (:active props) "x" " ") "] " (or (:label props) ""))]
              (scr/put! screen (:x rect) (:y rect) (text/pad s (:w rect))
                        (style props (= (:id n) (:focus-id ctx))))))
   :focusable? (fn [_] true)
   ;; The handler owns the state, exactly as in glimmer-gtk: it flips the cell
   ;; the component reads, the component re-renders, and :active comes back down
   ;; as a prop. The widget never toggles itself.
   :activate (fn [w] (when-let [f (:on-toggled (:props @w))] (f)))})

;; An entry keeps its edit buffer in :state — see the namespace docstring for why
;; that cannot live in props.
(defn- entry-value [n] (or (:value (:state n)) ""))

(defn- entry-spec []
  {:container :none
   :measure (fn [props _] {:w (or (:width-request props) 20) :h 1})
   :paint
   (fn [screen rect props n ctx]
     (let [focused? (= (:id n) (:focus-id ctx))
           v (entry-value n)
           ;; scroll the buffer horizontally so the caret stays in view
           caret (min (:cursor (:state n) (count v)) (count v))
           w (:w rect)
           start (max 0 (- caret (dec w)))
           shown (subs v (min start (count v)))
           empty-hint (and (empty? v) (:placeholder props))
           body (if empty-hint (str (:placeholder props)) shown)
           st (cond-> (style props focused?)
                empty-hint (assoc :dim true))]
       (scr/put! screen (:x rect) (:y rect) (text/pad body w)
                 (if focused? st (assoc st :underline true)))))
   :focusable? (fn [_] true)
   :cursor (fn [rect _ state]
             (let [v (or (:value state) "")
                   caret (min (:cursor state (count v)) (count v))
                   start (max 0 (- caret (dec (:w rect))))]
               [(+ (:x rect) (text/width (subs v (min start (count v))
                                               (min caret (count v)))))
                (:y rect)]))
   :activate (fn [w] (when-let [f (:on-activate (:props @w))] (f)))
   :key
   (fn [w code]
     (let [{:keys [state props]} @w
           v (or (:value state) "")
           caret (min (:cursor state (count v)) (count v))
           emit! (fn [nv] (when-let [f (:on-change props)] (f nv)))
           commit! (fn [nv nc]
                  (swap! w assoc :state (assoc state :value nv :cursor nc))
                  (touch!)
                  (emit! nv)
                  true)]
       (cond
         (= code 263) (if (pos? caret)                     ; KEY_BACKSPACE
                        (commit! (str (subs v 0 (dec caret)) (subs v caret)) (dec caret))
                        true)
         (= code 127) (if (pos? caret)                     ; DEL, what most terminals send
                        (commit! (str (subs v 0 (dec caret)) (subs v caret)) (dec caret))
                        true)
         (= code 330) (if (< caret (count v))              ; KEY_DC
                        (commit! (str (subs v 0 caret) (subs v (inc caret))) caret)
                        true)
         (= code 260) (do (swap! w assoc-in [:state :cursor] (max 0 (dec caret)))  ; left
                          (touch!) true)
         (= code 261) (do (swap! w assoc-in [:state :cursor] (min (count v) (inc caret)))
                          (touch!) true)                                          ; right
         (= code 262) (do (swap! w assoc-in [:state :cursor] 0) (touch!) true)     ; home
         (= code 360) (do (swap! w assoc-in [:state :cursor] (count v)) (touch!) true) ; end
         ;; printable character
         (and (>= code 32) (< code 127))
         (commit! (str (subs v 0 caret) (char code) (subs v caret)) (inc caret))
         :else false)))})

(defn- separator-spec []
  {:container :none
   :measure (fn [_ _] {:w 1 :h 1})
   :paint (fn [screen rect props _ _]
            (let [st (style props)
                  vertical? (= :vertical (:orientation props))]
              (if vertical?
                (doseq [i (range (:h rect))]
                  (scr/put! screen (:x rect) (+ (:y rect) i) "│" st))
                (doseq [i (range (:h rect))]
                  (scr/put! screen (:x rect) (+ (:y rect) i)
                            (apply str (repeat (:w rect) "─")) st)))))})

(defn- box-spec []
  {:container :box
   :measure (fn [_ _] {:w 0 :h 0})
   :paint (fn [screen rect props _ _]
            ;; a box only paints its background, so a nested box does not punch a
            ;; hole in what is underneath it
            (when (:bg props)
              (doseq [i (range (:h rect))]
                (scr/fill! screen (:x rect) (+ (:y rect) i) (:w rect) (style props)))))})

(defn- frame-spec []
  {:container :frame
   :measure (fn [_ _] {:w 2 :h 2})
   :paint
   (fn [screen rect props _ _]
     (let [{:keys [x y w h]} rect
           st (style props)]
       (when (and (>= w 2) (>= h 2))
         (let [top (str "┌" (apply str (repeat (- w 2) "─")) "┐")
               bottom (str "└" (apply str (repeat (- w 2) "─")) "┘")]
           (scr/put! screen x y top st)
           (scr/put! screen x (+ y (dec h)) bottom st)
           (doseq [i (range 1 (dec h))]
             (scr/put! screen x (+ y i) "│" st)
             (scr/put! screen (+ x (dec w)) (+ y i) "│" st))
           ;; the title sits in the top border, like a GtkFrame label
           (when-let [l (:label props)]
             (let [t (text/truncate (str " " l " ") (max 0 (- w 2)))]
               (scr/put! screen (+ x 1) y t st)))))))})

(defn- window-spec []
  {:container :window
   :measure (fn [_ _] {:w 0 :h 0})
   :paint (fn [_ _ _ _ _] nil)})

(reset! specs
        {:window      (window-spec)
         :box         (box-spec)
         :label       (label-spec)
         :button      (button-spec)
         :entry       (entry-spec)
         :checkbutton (checkbutton-spec)
         :separator   (separator-spec)
         :frame       (frame-spec)})

;; --- backend operations ------------------------------------------------------
(defn create!
  "Construct a widget for `tag` with `props`. Pure data: nothing is drawn until
  the event loop's next repaint."
  [tag props]
  (let [props (with-orientation tag props)
        w (node (normalize-tag tag) props)]
    ;; an entry starts holding whatever text the component gave it
    (when (= :entry (normalize-tag tag))
      (swap! w assoc :state {:value (str (or (:text props) ""))
                             :cursor (count (str (or (:text props) "")))}))
    (touch!)
    w))

(defn apply-props!
  "Re-apply props to an existing widget (the re-render path).

  An entry's edit buffer is only overwritten when the incoming :text differs
  from what the widget currently holds. Without that check, the :on-change ->
  swap! -> re-render cycle would write the buffer back over itself and park the
  caret at the end on every keystroke — the terminal equivalent of the
  suppressing setters in glimmer-gtk."
  [tag w props]
  (let [props (with-orientation tag props)]
    (swap! w assoc :props props)
    (when (= :entry (normalize-tag tag))
      (let [incoming (:text props)
            current (:value (:state @w))]
        (when (and (some? incoming) (not= (str incoming) current))
          (swap! w assoc :state {:value (str incoming) :cursor (count (str incoming))}))))
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

;; --- tree queries (used by focus and mouse handling) -------------------------
(defn walk
  "Depth-first sequence of nodes in a snapshot, parents before children."
  [n]
  (cons n (mapcat walk (:children n))))

(defn focus-ring
  "Ids of the focusable nodes in `snapshot`, in tab order (tree order)."
  [snapshot]
  (vec (keep (fn [n] (when (focusable? n) (:id n))) (walk snapshot))))

(defn hit
  "The innermost focusable node in a laid-out `snapshot` whose rect contains the
  cell (x, y), or nil. Later siblings win, matching paint order."
  [snapshot x y]
  (let [inside? (fn [{:keys [rect]}]
                  (and rect
                       (>= x (:x rect)) (< x (+ (:x rect) (:w rect)))
                       (>= y (:y rect)) (< y (+ (:y rect) (:h rect)))))]
    (last (filter (fn [n] (and (inside? n) (focusable? n))) (walk snapshot)))))

(defn find-widget
  "The live widget atom with `id` inside the tree rooted at `w`, or nil. The
  snapshot is what gets laid out and hit-tested, but activating a widget has to
  reach the atom the handlers live on."
  [w id]
  (when w
    (if (= id (:id @w))
      w
      (some #(find-widget % id) (:children @w)))))
