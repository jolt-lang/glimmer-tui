(ns glimmer-tui.widgets.indicators
  "Widgets that report progress rather than accept input: a progress bar, a
  spinner and a page indicator.

  None of them animate themselves. A spinner's frame is a prop, which means the
  component owns it like every other piece of state, and the loop's `every!`
  drives it:

    (let [tick (r/atom 0)]
      (tui/every! 80 #(swap! tick inc))
      (fn [] [:spinner {:tick @tick :label \"working\"}]))

  A widget that ran its own timer would repaint whether or not anything was
  looking at it, and would have to be told to stop."
  (:require [glimmer-tui.screen :as scr]
            [glimmer-tui.text :as text]
            [glimmer-tui.widget :as w]))

;; --- progress ----------------------------------------------------------------
(def bar-styles
  "Fill and empty characters by name. :ascii is there for a terminal that cannot
  draw the rest."
  {:blocks   {:full "█" :empty "░"}
   :thick    {:full "█" :empty "▒"}
   :thin     {:full "━" :empty "─"}
   :shaded   {:full "▓" :empty "░"}
   :dots     {:full "●" :empty "○"}
   :arrows   {:full ">" :empty " " :left "[" :right "]"}
   :brackets {:full "=" :empty " " :left "[" :right "]"}
   :ascii    {:full "#" :empty "-"}})

(defn- fraction [props]
  (let [v (or (:value props) 0)]
    (max 0.0 (min 1.0 (double v)))))

(defn- percent-label [props]
  (when (:show-percent props)
    (str " " (Math/round (* 100.0 (fraction props))) "%")))

(def progress-spec
  {:container :none
   :measure (fn [props _]
              {:w (+ (or (:width-request props) 20)
                     (text/width (str (percent-label props))))
               :h 1})
   :paint
   (fn [screen rect props _ _]
     (let [style-name (:bar props :blocks)
           bar (get bar-styles style-name (bar-styles :blocks))
           label (str (percent-label props))
           left (:left bar "")
           right (:right bar "")
           room (max 0 (- (:w rect) (text/width label)
                          (text/width left) (text/width right)))
           filled (Math/round (* room (fraction props)))
           st (w/style props)
           bar-style (if (:bar-color props) (assoc st :fg (:bar-color props)) st)]
       (scr/put! screen (:x rect) (:y rect) left st)
       (scr/put! screen (+ (:x rect) (text/width left)) (:y rect)
                 (apply str (repeat filled (:full bar))) bar-style)
       (scr/put! screen (+ (:x rect) (text/width left) filled) (:y rect)
                 (apply str (repeat (- room filled) (:empty bar)))
                 (assoc st :dim true))
       (scr/put! screen (+ (:x rect) (text/width left) room) (:y rect)
                 (str right label) st)))})

;; --- spinner -----------------------------------------------------------------
(def spinner-frames
  "Frame sets, in the order they animate."
  {:dots     ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
   :line     ["|" "/" "-" "\\"]
   :arrow    ["←" "↖" "↑" "↗" "→" "↘" "↓" "↙"]
   :bounce   ["⠁" "⠂" "⠄" "⠂"]
   :box      ["▖" "▘" "▝" "▗"]
   :circle   ["◐" "◓" "◑" "◒"]
   :pulse    ["█" "▓" "▒" "░" "▒" "▓"]
   :points   ["∙∙∙" "●∙∙" "∙●∙" "∙∙●"]
   :ascii    ["-" "\\" "|" "/"]})

(defn- frames-of [props]
  (let [f (:frames props :dots)]
    (if (sequential? f) (vec f) (get spinner-frames f (spinner-frames :dots)))))

(defn- current-frame [props]
  (let [fs (frames-of props)]
    (nth fs (mod (or (:tick props) 0) (count fs)))))

(def spinner-spec
  {:container :none
   :measure (fn [props _]
              (let [fs (frames-of props)
                    widest (reduce max 0 (map text/width fs))
                    label (str (:label props))]
                {:w (+ widest (if (seq label) (inc (text/width label)) 0)) :h 1}))
   :paint (fn [screen rect props _ _]
            (let [label (str (:label props))
                  s (if (seq label)
                      (str (current-frame props) " " label)
                      (current-frame props))]
              (scr/put! screen (:x rect) (:y rect) (text/pad s (:w rect))
                        (w/style props))))})

;; --- paginator ---------------------------------------------------------------
(defn- pages [props]
  (let [total (:total-pages props)]
    (max 1 (or total
               (let [items (or (:total-items props) 0)
                     per (max 1 (or (:per-page props) 10))]
                 (int (Math/ceil (/ (double items) per))))))))

(defn- paginator-text [props]
  (let [n (pages props)
        page (max 0 (min (dec n) (or (:page props) 0)))]
    (case (:style props :dots)
      :arabic (str (inc page) "/" n)
      (apply str (map #(if (= % page)
                         (:active-dot props "●")
                         (:inactive-dot props "○"))
                      (range n))))))

(def paginator-spec
  {:container :none
   :measure (fn [props _] {:w (text/width (paginator-text props)) :h 1})
   :paint (fn [screen rect props _ _]
            (scr/put! screen (:x rect) (:y rect)
                      (w/text-align (paginator-text props) (:w rect)
                                    (:align props :start))
                      (w/style props)))})

(defn install! []
  (w/register-widget! :progress progress-spec)
  (w/register-widget! :spinner spinner-spec)
  (w/register-widget! :paginator paginator-spec)
  nil)
