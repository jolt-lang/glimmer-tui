(ns glimmer-tui.border
  "Box-drawing sets for framed widgets.

  A border is eight strings — four edges and four corners — so a widget draws a
  frame without knowing which characters are in it. `:ascii` exists for the
  terminal that cannot show the rest: a serial console, a Windows console host
  in a codepage that predates Unicode, or `TERM=vt100`."
  (:refer-clojure :exclude [name]))

(defn border
  "A border from its eight parts. Anything omitted is a space, which is what
  makes a partial border (say, only a top edge) come out aligned."
  [{:keys [top bottom left right top-left top-right bottom-left bottom-right]}]
  {:top (or top " ") :bottom (or bottom " ")
   :left (or left " ") :right (or right " ")
   :top-left (or top-left " ") :top-right (or top-right " ")
   :bottom-left (or bottom-left " ") :bottom-right (or bottom-right " ")})

(def normal
  (border {:top "─" :bottom "─" :left "│" :right "│"
           :top-left "┌" :top-right "┐" :bottom-left "└" :bottom-right "┘"}))

(def rounded
  (border {:top "─" :bottom "─" :left "│" :right "│"
           :top-left "╭" :top-right "╮" :bottom-left "╰" :bottom-right "╯"}))

(def thick
  (border {:top "━" :bottom "━" :left "┃" :right "┃"
           :top-left "┏" :top-right "┓" :bottom-left "┗" :bottom-right "┛"}))

(def double-line
  (border {:top "═" :bottom "═" :left "║" :right "║"
           :top-left "╔" :top-right "╗" :bottom-left "╚" :bottom-right "╝"}))

(def block
  (border {:top "█" :bottom "█" :left "█" :right "█"
           :top-left "█" :top-right "█" :bottom-left "█" :bottom-right "█"}))

(def outer-half-block
  (border {:top "▀" :bottom "▄" :left "▌" :right "▐"
           :top-left "▛" :top-right "▜" :bottom-left "▙" :bottom-right "▟"}))

(def inner-half-block
  (border {:top "▄" :bottom "▀" :left "▐" :right "▌"
           :top-left "▗" :top-right "▖" :bottom-left "▝" :bottom-right "▘"}))

(def ascii
  (border {:top "-" :bottom "-" :left "|" :right "|"
           :top-left "+" :top-right "+" :bottom-left "+" :bottom-right "+"}))

(def hidden
  "Spaces: the frame still costs its two cells, so a hidden border and a visible
  one lay out identically. Use it to keep a row of frames aligned when one of
  them has nothing to say."
  (border {}))

(def styles
  "Border sets by name, for a `:border` prop."
  {:normal normal :rounded rounded :thick thick :double double-line
   :block block :outer-half-block outer-half-block
   :inner-half-block inner-half-block :ascii ascii :hidden hidden})

(defn resolve-border
  "The border set named by `b`, which may already be one. nil means :normal, and
  :none means no border at all (nil, so the caller can skip drawing)."
  [b]
  (cond
    (nil? b) normal
    (= :none b) nil
    (keyword? b) (get styles b normal)
    (map? b) (merge normal b)
    :else normal))
