(ns glimmer-tui.text
  "Display width of text in terminal cells, and the truncation and padding built
  on it. Pure, so layout stays testable without a terminal.

  A cell is not a character: combining marks take no width, and CJK ideographs,
  Hangul, kana and most emoji take two. Getting this wrong shifts everything to
  the right of the offending glyph, so layout measures with `width` rather than
  counting characters. The ranges below are the wide/zero-width blocks of
  Unicode's East Asian Width property that actually turn up in a UI; this is a
  deliberate approximation of wcwidth(3) that avoids an FFI call in the layout
  path (and the locale dependence that comes with it)."
  (:require [clojure.string :as str]))

(defn- in-range? [c lo hi] (and (>= c lo) (<= c hi)))

(defn char-width
  "Cells occupied by the character with code point `c`: 0 for a combining mark
  or a control character, 2 for a wide (East Asian W/F) glyph, 1 otherwise."
  [c]
  (cond
    (< c 32) 0                                    ; C0 controls
    (in-range? c 0x7f 0x9f) 0                     ; DEL + C1 controls
    (in-range? c 0x300 0x36f) 0                   ; combining diacriticals
    (in-range? c 0x200b 0x200f) 0                 ; zero-width space/joiners
    (= c 0xfeff) 0                                ; BOM / zero-width no-break
    (in-range? c 0x1100 0x115f) 2                 ; Hangul Jamo
    (in-range? c 0x2e80 0x303e) 2                 ; CJK radicals, Kangxi
    (in-range? c 0x3041 0x33ff) 2                 ; kana, CJK compatibility
    (in-range? c 0x3400 0x4dbf) 2                 ; CJK extension A
    (in-range? c 0x4e00 0x9fff) 2                 ; CJK unified ideographs
    (in-range? c 0xa000 0xa4cf) 2                 ; Yi
    (in-range? c 0xac00 0xd7a3) 2                 ; Hangul syllables
    (in-range? c 0xf900 0xfaff) 2                 ; CJK compatibility ideographs
    (in-range? c 0xfe30 0xfe6f) 2                 ; CJK compatibility forms
    (in-range? c 0xff00 0xff60) 2                 ; fullwidth forms
    (in-range? c 0xffe0 0xffe6) 2                 ; fullwidth signs
    (in-range? c 0x1f300 0x1f64f) 2               ; emoji, pictographs
    (in-range? c 0x1f900 0x1f9ff) 2               ; supplemental emoji
    (in-range? c 0x20000 0x3fffd) 2               ; CJK extension B and beyond
    :else 1))

(defn width
  "Cells occupied by string `s`."
  [s]
  (reduce (fn [n c] (+ n (char-width (int c)))) 0 (or s "")))

(defn truncate
  "The longest prefix of `s` that fits in `cells` columns. A wide glyph that
  would straddle the edge is dropped rather than half-drawn."
  [s cells]
  (if (<= (width s) cells)
    (or s "")
    (loop [chars (seq s) used 0 acc []]
      (if-let [c (first chars)]
        (let [w (char-width (int c))]
          (if (> (+ used w) cells)
            (apply str acc)
            (recur (next chars) (+ used w) (conj acc c))))
        (apply str acc)))))

(defn pad
  "`s` truncated to `cells` and then space-padded to exactly that width, so a
  repaint overwrites whatever was underneath it."
  [s cells]
  (let [t (truncate s cells)
        n (- cells (width t))]
    (str t (apply str (repeat (max 0 n) " ")))))

(defn lines
  "Split `s` on newlines. A label is measured and painted line by line, so an
  embedded newline makes it taller rather than corrupting the row."
  [s]
  (if (empty? s) [""] (vec (str/split s #"\n" -1))))

(defn block-width
  "The widest line in `s`."
  [s]
  (reduce max 0 (map width (lines s))))
