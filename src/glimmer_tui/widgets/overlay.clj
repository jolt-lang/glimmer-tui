(ns glimmer-tui.widgets.overlay
  "A floating panel: a dialog, a menu, a tooltip.

  An overlay is written where it belongs in the component that owns it, and laid
  out against the SCREEN — it takes no space at its declaration site and is not
  clipped by the box it was declared in. The renderer paints overlays last, in
  declaration order, so a later one sits on top of an earlier one.

  While an overlay with :modal true (the default) is on screen the focus ring is
  restricted to it, so Tab cannot wander back into the form underneath, and Esc
  calls :on-close.

  Props:
    :anchor    :center (default), :top-left, :top-center, :top-right,
               :center-left, :center-right, :bottom-left, :bottom-center,
               :bottom-right
    :offset-x  :offset-y   cells to nudge it by after anchoring
    :modal     false to leave focus alone
    :on-close  called when Esc is pressed
    :bg        a background colour, which is what makes it look like a panel
               rather than text printed over text"
  (:require [glimmer-tui.screen :as scr]
            [glimmer-tui.widget :as w]))

(def overlay-spec
  {:container :overlay
   :measure (fn [_ _] {:w 0 :h 0})
   :paint (fn [screen rect props _ _]
            ;; an overlay always paints its background, even without :bg: it has
            ;; to erase what it is covering, or the text underneath shows through
            (let [st (w/style props)]
              (doseq [i (range (:h rect))]
                (scr/fill! screen (:x rect) (+ (:y rect) i) (:w rect) st))))})

(defn modal?
  "Whether `n` is an overlay that captures focus."
  [n]
  (and (= :overlay (w/container-kind (:tag n)))
       (not (false? (:modal (:props n))))))

(defn install! []
  (w/register-widget! :overlay overlay-spec)
  nil)
