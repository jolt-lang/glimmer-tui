(ns glimmer-tui.widgets.scroll
  "A viewport onto a child taller (or wider) than the room it has.

  Layout gives the child its full natural size and shifts it by the scroll
  offset; the renderer clips what falls outside. Nothing is re-measured while
  scrolling, so scrolling a thousand-row list costs the same as scrolling a
  ten-row one.

  Props:
    :orientation  :vertical (default), :horizontal or :both
    :scrollbar    false to give the column back (default true)
    :keys         overrides for the bindings below
    :on-scroll    called with {:x :y} after the offset changes

  A scroll takes keyboard focus only when nothing inside it can: a viewport full
  of text is the thing you are driving, while a scroll wrapped around a form is
  not — there, Tab moves between the fields and the scroll follows the focus on
  its own. Either way its bindings still work, because a key the focused widget
  declines is offered to its ancestors on the way out."
  (:require [glimmer-tui.keys :as k]
            [glimmer-tui.screen :as scr]
            [glimmer-tui.widget :as w]))

(def default-bindings
  [[:line-up        ["up" "k"]]
   [:line-down      ["down" "j"]]
   [:half-page-up   ["ctrl+u"]]
   [:half-page-down ["ctrl+d"]]
   [:page-up        ["pgup" "ctrl+b"]]
   [:page-down      ["pgdn" "ctrl+f"]]
   [:left           ["left" "h"]]
   [:right          ["right" "l"]]
   [:top            ["home" "g"]]
   [:bottom         ["end" "G"]]])

(defn- bindings [props] (k/merge-bindings default-bindings (:keys props)))

(defn- vertical? [props] (contains? #{:vertical :both nil} (:orientation props)))
(defn- horizontal? [props] (contains? #{:horizontal :both} (:orientation props)))

;; --- offsets -----------------------------------------------------------------
(defn max-offset
  "How far a laid-out scroll `node` can be scrolled on each axis."
  [node]
  {:x (max 0 (- (:w (:content node) 0) (:w (:viewport node) 0)))
   :y (max 0 (- (:h (:content node) 0) (:h (:viewport node) 0)))})

(defn offset
  "The scroll offset a laid-out `node` was arranged with."
  [node] (:offset node {:x 0 :y 0}))

(defn scroll-to!
  "Move `wid` to offset [x y], clamped to what `node` can actually show. Returns
  true when the offset changed."
  [wid node x y]
  (let [lim (max-offset node)
        nx (max 0 (min x (:x lim)))
        ny (max 0 (min y (:y lim)))
        {:keys [x-offset y-offset]} (:state @wid)]
    (if (and (= nx (or x-offset 0)) (= ny (or y-offset 0)))
      false
      (do (swap! wid update :state assoc :x-offset nx :y-offset ny)
          (w/touch!)
          (when-let [f (:on-scroll (:props @wid))] (f {:x nx :y ny}))
          true))))

(defn scroll-by! [wid node dx dy]
  (let [o (offset node)]
    (scroll-to! wid node (+ (:x o) dx) (+ (:y o) dy))))

(defn ensure-visible!
  "Scroll just enough that `rect` is inside the viewport of the laid-out scroll
  `node`. This is what keeps Tab usable in a form taller than the terminal: the
  focus ring is computed over the whole tree, so focus can land on a widget that
  is off-screen, and the scroll it lives in brings it back."
  [wid node rect]
  (let [view (:rect node)
        o (offset node)
        vw (:w (:viewport node) (:w view 0))
        vh (:h (:viewport node) (:h view 0))
        ;; rect is in screen coordinates, which already include the offset
        top (- (:y rect) (:y view))
        bottom (- (+ (:y rect) (:h rect)) (:y view))
        left (- (:x rect) (:x view))
        right (- (+ (:x rect) (:w rect)) (:x view))
        dy (cond
             (neg? top) top
             (> bottom vh) (min top (- bottom vh))
             :else 0)
        dx (cond
             (neg? left) left
             (> right vw) (min left (- right vw))
             :else 0)]
    (if (or (not= 0 dx) (not= 0 dy))
      (scroll-to! wid node (+ (:x o) dx) (+ (:y o) dy))
      false)))

;; --- keys --------------------------------------------------------------------
(defn- handle-key [wid event node]
  (let [props (:props @wid)
        vh (:h (:viewport node) 0)
        half (max 1 (quot vh 2))
        page (max 1 (dec vh))
        lim (max-offset node)
        v? (vertical? props)
        h? (horizontal? props)
        action (k/matches-any? event (bindings props))]
    (case action
      :line-up        (and v? (scroll-by! wid node 0 -1))
      :line-down      (and v? (scroll-by! wid node 0 1))
      :half-page-up   (and v? (scroll-by! wid node 0 (- half)))
      :half-page-down (and v? (scroll-by! wid node 0 half))
      :page-up        (and v? (scroll-by! wid node 0 (- page)))
      :page-down      (and v? (scroll-by! wid node 0 page))
      :left           (and h? (scroll-by! wid node -1 0))
      :right          (and h? (scroll-by! wid node 1 0))
      :top            (scroll-to! wid node 0 0)
      :bottom         (scroll-to! wid node 0 (:y lim))
      false)))

;; --- painting ----------------------------------------------------------------
(defn- thumb
  "[start length] of the scrollbar thumb for a viewport `view` cells tall onto
  `content` cells of content, scrolled to `off`."
  [view content off]
  (let [len (max 1 (quot (* view view) (max 1 content)))
        room (- view len)
        span (max 1 (- content view))
        start (if (pos? room) (quot (* off room) span) 0)]
    [(min start room) len]))

(def scroll-spec
  {:container :scroll
   ;; the natural size is the child's; layout reads it directly, so nothing to do
   :measure (fn [_ _] {:w 0 :h 0})
   :init-state (fn [props] {:x-offset (or (:x-offset props) 0)
                            :y-offset (or (:y-offset props) 0)})
   :bindings (fn [props] (bindings props))
   ;; only when there is nothing inside to focus instead
   :focusable? (fn [n]
                 (not (some w/focusable? (rest (w/walk n)))))
   :paint
   (fn [screen rect props n _]
     (when (:bg props)
       (doseq [i (range (:h rect))]
         (scr/fill! screen (:x rect) (+ (:y rect) i) (:w rect) (w/style props))))
     (let [content (:h (:content n) 0)
           view (:h (:viewport n) 0)]
       (when (and (not (false? (:scrollbar props))) (> content view) (pos? view))
         (let [[start len] (thumb view content (:y (:offset n {:y 0})))
               x (+ (:x rect) (dec (:w rect)))
               st (w/style props)]
           (doseq [i (range view)]
             (scr/put! screen x (+ (:y rect) i)
                       (if (and (>= i start) (< i (+ start len))) "█" "│")
                       st))))))
   :key (fn [wid event ctx] (boolean (handle-key wid event (:node ctx))))})

(defn install! []
  (w/register-widget! :scroll scroll-spec)
  nil)
