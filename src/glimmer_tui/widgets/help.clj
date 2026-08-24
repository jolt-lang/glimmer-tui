(ns glimmer-tui.widgets.help
  "A key-binding bar.

  With no :bindings prop it renders the bindings of whatever is focused, read off
  the live widget tree — a list says `↑/k up  ↓/j down  pgup page up`, and the
  same bar says something else once focus moves to a text field. That is the
  point of keeping bindings in the widget specs rather than in a comment: the
  help is generated from the thing it documents, so it cannot drift.

  Props:
    :bindings   [[key desc] …] or [{:key … :desc …} …], overriding the derived set
    :labels     {action \"description\"}, to rename the derived actions
    :separator  between entries (default \"  \")
    :full       true to put one binding per line instead of one bar"
  (:require [clojure.string :as str]
            [glimmer-tui.keys :as k]
            [glimmer-tui.screen :as scr]
            [glimmer-tui.text :as text]
            [glimmer-tui.widget :as w]))

(defn- humanise [action]
  (str/replace (name action) "-" " "))

(defn- entries
  "The [key desc] pairs to draw: the :bindings prop if given, otherwise whatever
  the focused widget answers to, which the event loop pushes into :state."
  [props state]
  (let [given (:bindings props)]
    (cond
      (seq given)
      (mapv (fn [b] (if (map? b)
                      [(str (:key b)) (str (:desc b))]
                      [(str (first b)) (str (second b))]))
            given)

      :else
      (let [labels (:labels props)]
        (mapv (fn [[action spec]]
                [(k/describe spec) (or (get labels action) (humanise action))])
              (:bindings state))))))

(defn- bar
  "The single-line form, cut with an ellipsis rather than in the middle of a
  word when there is not room for all of it."
  [props state cells]
  (let [sep (:separator props "  ")
        s (str/join sep (map (fn [[key desc]] (str key " " desc)) (entries props state)))
        ellipsis (:ellipsis props "…")]
    (if (or (<= cells 0) (<= (text/width s) cells))
      s
      (str (text/truncate s (max 0 (- cells (text/width ellipsis)))) ellipsis))))

(def help-spec
  {:container :none
   ;; the bindings in effect are not a prop and not derivable from one, so the
   ;; loop pushes them in and both measure and paint read them from :state
   :derive-state (fn [_ state ctx]
                   (if (= (:bindings state) (:bindings ctx))
                     state
                     (assoc state :bindings (:bindings ctx))))
   :measure (fn [props state]
              {:w (or (:width-request props) 0)
               :h (if (:full props) (max 1 (count (entries props state))) 1)})
   :paint
   (fn [screen rect props n _]
     (let [state (:state n)
           st (assoc (w/style props) :dim (not (false? (:dim props))))
           key-style (cond-> st (:key-color props) (assoc :fg (:key-color props)))]
       (if (:full props)
         ;; one binding per line, keys in a column of their own
         (let [rows (entries props state)
               gutter (reduce max 0 (map (fn [[key _]] (text/width key)) rows))]
           (doseq [[i [key desc]] (map-indexed vector rows)
                   :while (< i (:h rect))]
             (scr/put! screen (:x rect) (+ (:y rect) i)
                       (text/pad (str (w/text-align key gutter :start) "  " desc)
                                 (:w rect))
                       st)))
         (scr/put! screen (:x rect) (:y rect)
                   (text/pad (bar props state (:w rect)) (:w rect)) key-style))))})

(defn install! []
  (w/register-widget! :help help-spec)
  nil)
