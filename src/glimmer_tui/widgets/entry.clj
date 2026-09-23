(ns glimmer-tui.widgets.entry
  "A single-line text field.

  The edit buffer lives in the widget's :state rather than in props, because the
  component cannot own a caret: it re-renders on every keystroke, and writing
  :text back into the widget would park the caret at the end each time. :text
  therefore only overwrites the buffer when it differs from what the widget is
  already holding — see :sync-state below.

  Props:
    :text          the value the component believes the field holds
    :placeholder   dim text shown while the field is empty
    :echo          :normal (default), :password, or :none
    :echo-char     what :password draws (default \"•\")
    :char-limit    refuse insertions past this many characters
    :width-request the field's natural width in cells (default 20)
    :keys          overrides for the bindings below
    :on-change     called with the new text
    :on-activate   called on Enter
    :on-paste      called with the pasted text INSTEAD of inserting it, for a
                   field that wants to decide what a paste means

  A paste arrives as one event rather than as its characters (see
  glimmer-tui.core), so a pasted newline no longer activates the field. Without
  an :on-paste the text is inserted at the caret with its control characters —
  a newline above all — flattened to spaces, because a one-line field has
  nowhere to put a line break and ncurses would act on it rather than draw it.
  An app that wants the line breaks takes :on-paste and keeps the text itself.

  The bindings are readline's, which is what a terminal user's fingers already
  know: ctrl-a/ctrl-e for the ends, ctrl-w to rub out a word, ctrl-u and ctrl-k
  to cut to either end, alt-b/alt-f to move by words."
  (:require [glimmer-tui.keys :as k]
            [glimmer-tui.screen :as scr]
            [glimmer-tui.text :as text]
            [glimmer-tui.widget :as w]))

(def default-bindings
  [[:backward-char ["left" "ctrl+b"]]
   [:forward-char  ["right" "ctrl+f"]]
   [:backward-word ["alt+b"]]
   [:forward-word  ["alt+f"]]
   [:cursor-start  ["home" "ctrl+a"]]
   [:cursor-end    ["end" "ctrl+e"]]
   [:delete-back   ["backspace"]]
   [:delete-fwd    ["delete" "ctrl+d"]]
   [:delete-word-back ["ctrl+w" "alt+backspace"]]
   [:delete-word-fwd  ["alt+d"]]
   [:delete-to-start  ["ctrl+u"]]
   [:delete-to-end    ["ctrl+k"]]])

(defn- bindings [props] (k/merge-bindings default-bindings (:keys props)))

(defn- value [state] (or (:value state) ""))

(defn- caret [state]
  (min (:cursor state (count (value state))) (count (value state))))

;; --- word motion -------------------------------------------------------------
(defn- space? [c] (contains? #{\space \tab} c))

(defn- word-start
  "The index of the start of the word at or before `i` — skip any run of spaces,
  then the run of non-spaces before it."
  [s i]
  (let [i (loop [i i] (if (and (pos? i) (space? (nth s (dec i)))) (recur (dec i)) i))]
    (loop [i i] (if (and (pos? i) (not (space? (nth s (dec i))))) (recur (dec i)) i))))

(defn- word-end
  "The index just past the end of the word at or after `i`."
  [s i]
  (let [n (count s)
        i (loop [i i] (if (and (< i n) (space? (nth s i))) (recur (inc i)) i))]
    (loop [i i] (if (and (< i n) (not (space? (nth s i)))) (recur (inc i)) i))))

;; --- editing -----------------------------------------------------------------
(defn- edit
  "Write `v` and caret `c` back to `wid`, fire :on-change if the text changed."
  [wid v c]
  (let [{:keys [props state]} @wid
        changed? (not= v (value state))]
    (swap! wid assoc :state (assoc state :value v :cursor (max 0 (min c (count v)))))
    (w/touch!)
    (when (and changed? (:on-change props)) ((:on-change props) v))
    true))

(defn- move [wid c]
  (swap! wid assoc-in [:state :cursor] c)
  (w/touch!)
  true)

(defn- insert-text
  "Insert `s` at the caret, as much of it as the character limit leaves room for."
  [wid s]
  (let [{:keys [props state]} @wid
        v (value state)
        c (caret state)
        limit (:char-limit props)
        room (if limit (max 0 (- limit (count v))) (count s))
        s (subs s 0 (min (count s) room))]
    (if (empty? s)
      true
      (edit wid (str (subs v 0 c) s (subs v c)) (+ c (count s))))))

(defn- insert [wid ch] (insert-text wid (str ch)))

(defn- one-line
  "`s` with every control character turned into a space — see :on-paste above."
  [s]
  (apply str (map (fn [ch]
                    (let [c (int ch)]
                      (if (or (< c 32) (= c 127)) \space ch)))
                  s)))

(defn- paste [wid text]
  (let [f (:on-paste (:props @wid))]
    (if f
      (do (f text) true)
      (insert-text wid (one-line (or text ""))))))

(defn- cut [wid from to]
  (let [v (value (:state @wid))
        from (max 0 from)
        to (min (count v) to)]
    (if (< from to)
      (edit wid (str (subs v 0 from) (subs v to)) from)
      true)))

(defn- handle-binding [wid event]
  (let [{:keys [props state]} @wid
        v (value state)
        c (caret state)
        b (bindings props)
        action (k/matches-any? event b)]
    (case action
      :cursor-start (move wid 0)
      :cursor-end   (move wid (count v))
      :backward-char (move wid (max 0 (dec c)))
      :forward-char  (move wid (min (count v) (inc c)))
      :backward-word (move wid (word-start v c))
      :forward-word  (move wid (word-end v c))
      :delete-back   (cut wid (dec c) c)
      :delete-fwd    (cut wid c (inc c))
      :delete-word-back (cut wid (word-start v c) c)
      :delete-word-fwd  (cut wid c (word-end v c))
      :delete-to-start  (cut wid 0 c)
      :delete-to-end    (cut wid c (count v))
      nil (when (k/printable? event) (insert wid (k/char-of event))))))

(defn- handle-key [wid event]
  (if (= :paste (:type event))
    (paste wid (:text event))
    (handle-binding wid event)))

;; --- painting ----------------------------------------------------------------
(defn- echoed
  "What the field draws for `v` — the text itself, bullets, or nothing."
  [props v]
  (case (:echo props :normal)
    :password (apply str (repeat (count (text/clusters v)) (:echo-char props "•")))
    :none ""
    v))

(defn- window-start
  "How many cells of the value are scrolled off to the left, so that the caret
  stays inside a field `w` cells wide. A caret after the last character needs a
  cell of its own, so the value is treated as one cell wider when it ends there."
  [shown before w]
  (let [at (text/width before)
        wide (max (text/width shown) (inc at))]
    (if (<= wide w)
      0
      (max 0 (min (- wide w) (- at (dec w)))))))

(def entry-spec
  {:container :none
   :measure (fn [props _] {:w (or (:width-request props) 20) :h 1})
   :init-state (fn [props]
                 (let [t (str (or (:text props) ""))]
                   {:value t :cursor (count t)}))
   :sync-state (fn [props state]
                 (let [incoming (:text props)]
                   (if (and (some? incoming) (not= (str incoming) (:value state)))
                     {:value (str incoming) :cursor (count (str incoming))}
                     state)))
   :bindings (fn [props] (bindings props))
   :paint
   (fn [screen rect props n ctx]
     (let [focused? (= (:id n) (:focus-id ctx))
           state (:state n)
           v (value state)
           shown (echoed props v)
           before (echoed props (subs v 0 (caret state)))
           width (:w rect)
           start (window-start shown before width)
           empty-hint (and (empty? v) (:placeholder props))
           body (if empty-hint (str (:placeholder props)) (text/drop-cells shown start))
           st (cond-> (w/style props focused?)
                empty-hint (assoc :dim true))]
       (scr/put! screen (:x rect) (:y rect) (text/pad body width)
                 (if focused? st (assoc st :underline true)))))
   :focusable? (fn [_] true)
   :cursor (fn [rect props state]
             (let [v (value state)
                   shown (echoed props v)
                   before (echoed props (subs v 0 (caret state)))
                   start (window-start shown before (:w rect))]
               [(+ (:x rect) (- (text/width before) start)) (:y rect)]))
   :activate (fn [wid] (when-let [f (:on-activate (:props @wid))] (f)))
   :key (fn [wid event _ctx] (boolean (handle-key wid event)))})

(defn install! []
  (w/register-widget! :entry entry-spec)
  nil)
