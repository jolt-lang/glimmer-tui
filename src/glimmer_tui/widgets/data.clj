(ns glimmer-tui.widgets.data
  "The two widgets that show a sequence of things: a list and a table.

  Both keep a cursor and a scroll offset in :state and both follow the same
  convention as every other glimmer widget — the HANDLER owns the selection. A
  key moves the cursor, :on-select fires with the new index, and if the component
  passes :selected back down that is what the widget shows. A component that
  ignores :on-select still gets a usable list, because the cursor falls back to
  the widget's own state.

  Neither needs a :scroll around it: the natural height is the whole content, so
  a box that cannot give them that much leaves them short and they scroll the
  cursor into view themselves."
  (:require [glimmer-tui.keys :as k]
            [glimmer-tui.screen :as scr]
            [glimmer-tui.text :as text]
            [glimmer-tui.widget :as w]))

(def default-bindings
  [[:up      ["up" "k" "ctrl+p"]]
   [:down    ["down" "j" "ctrl+n"]]
   [:page-up ["pgup" "ctrl+u"]]
   [:page-down ["pgdn" "ctrl+d"]]
   [:top     ["home" "g"]]
   [:bottom  ["end" "G"]]])

(defn- bindings [props] (k/merge-bindings default-bindings (:keys props)))

(defn- items [props] (vec (:items props)))

(defn- item-label [item]
  (cond
    (map? item) (str (or (:label item) (:text item) ""))
    :else (str item)))

(defn- item-desc [item] (when (map? item) (:desc item)))

(defn- rows-per-item [props] (if (:descriptions props) 2 1))

(defn- cursor-of [n]
  (let [state (:state n)
        n-items (count (items (:props n)))]
    (max 0 (min (dec (max 1 n-items)) (or (:cursor state) 0)))))

;; --- shared cursor movement --------------------------------------------------
(defn- visible-rows
  "How many items fit in the laid-out `node`, given how tall a row is."
  [node per]
  (max 1 (quot (:h (:rect node) 1) per)))

(defn- clamp-offset
  "Keep the cursor inside the window of rows the widget can actually draw."
  [offset cursor visible total]
  (let [offset (min offset (max 0 (- total visible)))
        offset (max 0 offset)]
    (cond
      (< cursor offset) cursor
      (>= cursor (+ offset visible)) (inc (- cursor visible))
      :else offset)))

(defn- move-cursor!
  "Put the cursor at `to`, wrapping when :wrap is on, adjust the offset so it is
  visible, and tell the component. `visible` is how many items the widget can
  draw at once, which is the caller's business: a table spends a row on its
  header and a list with descriptions spends two on every item."
  [wid node visible rows to]
  (let [{:keys [props state]} @wid
        rows (vec rows)
        total (count rows)]
    (when (pos? total)
      (let [to (if (:wrap props)
                 (mod to total)
                 (max 0 (min (dec total) to)))
            offset (clamp-offset (or (:offset state) 0) to visible total)]
        (when (or (not= to (:cursor state)) (not= offset (:offset state)))
          (swap! wid update :state assoc :cursor to :offset offset)
          (w/touch!)
          (when-let [f (:on-select props)]
            (f to (nth rows to nil))))))
    true))

(defn- nav
  "Interpret a navigation key against `total` items. Returns the new cursor
  index, or nil when the key was not one of ours."
  [event props cursor total visible]
  (case (k/matches-any? event (bindings props))
    :up (dec cursor)
    :down (inc cursor)
    :page-up (- cursor visible)
    :page-down (+ cursor visible)
    :top 0
    :bottom (dec total)
    nil))

;; --- listbox -----------------------------------------------------------------
(defn- listbox-row
  "The text of row `i` of the list — the item, or its description line."
  [props item selected? desc-row?]
  (let [prefix (if selected? (:cursor-prefix props "> ") (:item-prefix props "  "))]
    (if desc-row?
      (str (apply str (repeat (text/width prefix) " ")) (or (item-desc item) ""))
      (str prefix (item-label item)))))

(def listbox-spec
  {:container :none
   :measure
   (fn [props _]
     (let [is (items props)
           per (rows-per-item props)
           prefix (max (text/width (:cursor-prefix props "> "))
                       (text/width (:item-prefix props "  ")))
           widest (reduce max 0 (map (fn [i]
                                       (max (text/width (item-label i))
                                            (text/width (str (item-desc i)))))
                                     is))]
       {:w (+ prefix widest) :h (max 1 (* per (count is)))}))
   ;; a list keeps its own offset, so it can be given one row and still work —
   ;; which is what stops it from pushing the rest of the window off the bottom
   :min-size (fn [_ _ natural] {:w (:w natural) :h 1})
   :init-state (fn [props] {:cursor (or (:selected props) 0) :offset 0})
   :sync-state (fn [props state]
                 (let [sel (:selected props)]
                   (if (and (some? sel) (not= sel (:cursor state)))
                     (assoc state :cursor sel)
                     state)))
   :bindings (fn [props] (bindings props))
   :focusable? (fn [_] true)
   :paint
   (fn [screen rect props n ctx]
     (let [is (items props)
           per (rows-per-item props)
           cursor (cursor-of n)
           offset (or (:offset (:state n)) 0)
           focused? (= (:id n) (:focus-id ctx))
           visible (max 1 (quot (:h rect) per))]
       (doseq [row (range (:h rect))]
         (let [idx (+ offset (quot row per))
               desc? (and (:descriptions props) (odd? (rem row per)))
               item (nth is idx nil)
               selected? (= idx cursor)
               st (cond-> (w/style props)
                    (and selected? focused?) (assoc :reverse true)
                    (and selected? (not focused?)) (assoc :bold true))]
           (scr/put! screen (:x rect) (+ (:y rect) row)
                     (text/pad (if item (listbox-row props item selected? desc?) "")
                               (:w rect))
                     (if item st (w/style props)))))))
   :key
   (fn [wid event ctx]
     (let [props (:props @wid)
           node (:node ctx)
           total (count (items props))
           visible (visible-rows node (rows-per-item props))
           cursor (or (:cursor (:state @wid)) 0)]
       (if-let [to (nav event props cursor total visible)]
         (boolean (move-cursor! wid node visible (items props) to))
         false)))
   :click
   (fn [wid x y ctx]
     (let [node (:node ctx)
           props (:props @wid)
           per (rows-per-item props)
           offset (or (:offset (:state @wid)) 0)
           row (quot (- y (:y (:rect node))) per)]
       (move-cursor! wid node (visible-rows node per) (items props) (+ offset row))
       (when-let [f (:on-activate (:props @wid))] (f))))
   :activate (fn [wid] (when-let [f (:on-activate (:props @wid))] (f)))})

;; --- table -------------------------------------------------------------------
(defn- columns [props]
  (mapv (fn [c] (if (map? c) c {:title (str c) :key c})) (:columns props)))

(defn- cell-value [row col]
  (let [v (cond
            (map? row) (get row (:key col))
            (sequential? row) (nth (vec row) (:index col 0) nil)
            :else row)]
    (str (if (nil? v) "" v))))

(defn- column-widths
  "Each column's width: what it asked for, or the widest thing in it."
  [props]
  (let [cols (columns props)
        rows (vec (:rows props))]
    (mapv (fn [i col]
            (or (:width col)
                (reduce max (text/width (str (:title col)))
                        (map #(text/width (cell-value % (assoc col :index i))) rows))))
          (range (count cols))
          cols)))

(defn- table-row-text [props widths row]
  (let [cols (columns props)
        gap (:gap props 1)]
    (->> (map-indexed (fn [i col]
                        (let [v (cell-value row (assoc col :index i))
                              wide (nth widths i)]
                          (w/text-align v wide (:align col :start))))
                      cols)
         (interpose (apply str (repeat gap " ")))
         (apply str))))

(defn- header-text [props widths]
  (let [cols (columns props)
        gap (:gap props 1)]
    (->> (map-indexed (fn [i col] (w/text-align (str (:title col)) (nth widths i)
                                                (:align col :start)))
                      cols)
         (interpose (apply str (repeat gap " ")))
         (apply str))))

(defn- header? [props] (not (false? (:header props))))

(def table-spec
  {:container :none
   :measure
   (fn [props _]
     (let [widths (column-widths props)
           gap (:gap props 1)]
       {:w (+ (reduce + 0 widths) (* gap (max 0 (dec (count widths)))))
        :h (+ (count (:rows props)) (if (header? props) 1 0))}))
   ;; likewise: the header and one row are all a table truly needs
   :min-size (fn [props _ natural] {:w (:w natural) :h (if (header? props) 2 1)})
   :init-state (fn [props] {:cursor (or (:selected props) 0) :offset 0})
   :sync-state (fn [props state]
                 (let [sel (:selected props)]
                   (if (and (some? sel) (not= sel (:cursor state)))
                     (assoc state :cursor sel)
                     state)))
   :bindings (fn [props] (bindings props))
   :focusable? (fn [_] true)
   :paint
   (fn [screen rect props n ctx]
     (let [widths (column-widths props)
           rows (vec (:rows props))
           head? (header? props)
           cursor (max 0 (min (dec (max 1 (count rows))) (or (:cursor (:state n)) 0)))
           offset (or (:offset (:state n)) 0)
           focused? (= (:id n) (:focus-id ctx))
           base (w/style props)]
       (when head?
         (scr/put! screen (:x rect) (:y rect)
                   (text/pad (header-text props widths) (:w rect))
                   (assoc base :bold true)))
       (doseq [row (range (if head? (dec (:h rect)) (:h rect)))]
         (let [idx (+ offset row)
               r (nth rows idx nil)
               selected? (= idx cursor)
               st (cond-> base
                    (and selected? focused?) (assoc :reverse true)
                    (and selected? (not focused?)) (assoc :bold true))]
           (scr/put! screen (:x rect) (+ (:y rect) row (if head? 1 0))
                     (text/pad (if r (table-row-text props widths r) "") (:w rect))
                     (if r st base))))))
   :key
   (fn [wid event ctx]
     (let [props (:props @wid)
           node (:node ctx)
           total (count (:rows props))
           visible (max 1 (- (:h (:rect node) 1) (if (header? props) 1 0)))
           cursor (or (:cursor (:state @wid)) 0)]
       (if-let [to (nav event props cursor total visible)]
         (boolean (move-cursor! wid node visible (:rows props) to))
         false)))
   :click
   (fn [wid x y ctx]
     (let [node (:node ctx)
           props (:props @wid)
           visible (max 1 (- (:h (:rect node) 1) (if (header? props) 1 0)))
           offset (or (:offset (:state @wid)) 0)
           row (- y (:y (:rect node)) (if (header? props) 1 0))]
       (when (>= row 0)
         (move-cursor! wid node visible (:rows props) (+ offset row))
         (when-let [f (:on-activate props)] (f)))))
   :activate (fn [wid] (when-let [f (:on-activate (:props @wid))] (f)))})

(defn install! []
  (w/register-widget! :listbox listbox-spec)
  (w/register-widget! :table table-spec)
  nil)
