(ns glimmer-tui.widgets.frame
  "A single-child container that draws a border around it, with an optional
  title set into the top edge — a GtkFrame, in box-drawing characters.

  Props:
    :label        title text, drawn in the top border
    :label-align  :start (the default), :center or :end
    :border       :normal :rounded :thick :double :block :ascii :hidden :none,
                  or a map of the eight parts (see glimmer-tui.border)
    :border-color colour for the border itself, when it should differ from the
                  frame's :color"
  (:require [glimmer-tui.border :as border]
            [glimmer-tui.screen :as scr]
            [glimmer-tui.text :as text]
            [glimmer-tui.widget :as w]))

(defn- edge
  "`n` cells of the repeated edge character `ch`."
  [ch n] (apply str (repeat (max 0 n) ch)))

(def frame-spec
  {:container :frame
   :key (fn [wid event _ctx]
          (when-let [f (:on-key (:props @wid))] (boolean (f event))))
   :measure (fn [props _] (if (= :none (:border props)) {:w 0 :h 0} {:w 2 :h 2}))
   :paint
   (fn [screen rect props _ _]
     (when-let [b (border/resolve-border (:border props))]
       (let [{:keys [x y w h]} rect
             st (w/style props)
             bst (if (:border-color props) (assoc st :fg (:border-color props)) st)]
         (when (and (>= w 2) (>= h 2))
           (scr/put! screen x y
                     (str (:top-left b) (edge (:top b) (- w 2)) (:top-right b)) bst)
           (scr/put! screen x (+ y (dec h))
                     (str (:bottom-left b) (edge (:bottom b) (- w 2)) (:bottom-right b)) bst)
           (doseq [i (range 1 (dec h))]
             (scr/put! screen x (+ y i) (:left b) bst)
             (scr/put! screen (+ x (dec w)) (+ y i) (:right b) bst))
           ;; the title sits in the top border, like a GtkFrame label
           (when-let [l (:label props)]
             (let [room (max 0 (- w 2))
                   t (text/truncate (str " " l " ") room)
                   slack (- room (text/width t))
                   off (case (:label-align props :start)
                         :center (quot slack 2)
                         :end slack
                         0)]
               (scr/put! screen (+ x 1 off) y t st)))))))})

(defn install! []
  (w/register-widget! :frame frame-spec)
  nil)
