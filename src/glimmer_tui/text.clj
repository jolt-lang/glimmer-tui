(ns glimmer-tui.text
  "Display width of text in terminal cells, and the clustering, truncation and
  padding built on it. Pure, so layout stays testable without a terminal.

  A cell is not a character, and it is not a code point either. Combining marks,
  variation selectors, zero-width joiners and emoji modifiers all take no width
  of their own; CJK ideographs, Hangul, kana and most emoji take two. Getting
  this wrong shifts everything to the right of the offending glyph, so layout
  measures with `width` rather than counting characters.

  Text is therefore measured and cut in GRAPHEME CLUSTERS rather than code
  points: `👍🏽` is one cluster two cells wide, not a two-cell thumb followed by a
  stray skin tone, and `truncate` will never cut between the two. `clusters` is
  a deliberate approximation of UAX #29 covering what turns up in a UI —
  combining marks, variation selectors, ZWJ sequences, regional-indicator flag
  pairs and skin-tone modifiers.

  The width table is likewise a deliberate approximation of wcwidth(3), chosen
  over an FFI call in the layout path (and the locale dependence that comes with
  it). One place it departs from wcwidth on purpose: a cluster carrying U+FE0F
  (variation selector-16, \"render the previous character as emoji\") is two
  cells wide, because that is what a terminal actually draws, even though
  wcwidth reports the bare base character as one.

  Text that is entirely printable ASCII takes a fast path through all of this:
  it cannot hold a combining mark, a wide glyph or an emoji, so one character is
  one cluster is one cell and measuring it is counting it. That matters because
  a full-screen repaint measures the same labels several times a frame — during
  layout, again while clipping, again while aligning — and on an English UI
  every one of those measurements is this case."
  (:require [clojure.string :as str]))

(defn- in-range? [c lo hi] (and (>= c lo) (<= c hi)))

(def ^:private ZWJ 0x200d)
(def ^:private VS16 0xfe0f)

(defn- plain-text?
  "Whether every character of `s` is printable ASCII (0x20-0x7e): no control
  character, no combining mark, no wide glyph, no emoji — so `clusters` would
  return one character per cluster and `char-width` would call every one of them
  one cell, and the functions below can skip both.

  It stops at 0x7e rather than at 0x300, where the first combining mark lives,
  so that a control character still goes the long way round and `char-width`'s
  zero-width rule keeps deciding what it is worth."
  [s]
  (let [n (count s)]
    (loop [i 0]
      (if (>= i n)
        true
        (let [c (int (nth s i))]
          (if (or (< c 0x20) (> c 0x7e))
            false
            (recur (inc i))))))))

(defn char-width
  "Cells occupied by the character with code point `c`: 0 for a combining mark,
  a variation selector, an emoji modifier or a control character, 2 for a wide
  (East Asian W/F) glyph, 1 otherwise.

  This is the code-point rule. Use `width` for a string: a cluster is worth more
  than the sum of its parts."
  [c]
  (cond
    (< c 32) 0                                    ; C0 controls
    (in-range? c 0x7f 0x9f) 0                     ; DEL + C1 controls
    (in-range? c 0x300 0x36f) 0                   ; combining diacriticals
    (in-range? c 0x483 0x489) 0                   ; combining Cyrillic
    (in-range? c 0x1ab0 0x1aff) 0                 ; combining diacriticals extended
    (in-range? c 0x1dc0 0x1dff) 0                 ; combining diacriticals supplement
    (in-range? c 0x200b 0x200f) 0                 ; zero-width space/joiners
    (in-range? c 0x20d0 0x20f0) 0                 ; combining marks for symbols
    (= c 0xfeff) 0                                ; BOM / zero-width no-break
    (in-range? c 0xfe00 0xfe0f) 0                 ; variation selectors
    (in-range? c 0xfe20 0xfe2f) 0                 ; combining half marks
    (in-range? c 0x1f3fb 0x1f3ff) 0               ; emoji skin-tone modifiers
    (in-range? c 0xe0100 0xe01ef) 0               ; variation selectors supplement
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
    (in-range? c 0x1f680 0x1f6ff) 2               ; transport and map symbols
    (in-range? c 0x1f7e0 0x1f7eb) 2               ; geometric shapes extended
    (in-range? c 0x1f900 0x1f9ff) 2               ; supplemental emoji
    (in-range? c 0x1fa70 0x1faff) 2               ; symbols and pictographs extended-A
    (in-range? c 0x20000 0x3fffd) 2               ; CJK extension B and beyond
    :else 1))

(defn- regional? [c] (in-range? c 0x1f1e6 0x1f1ff))

(defn- joins-previous?
  "Whether code point `c` continues the cluster whose code points are `cur`."
  [cur c]
  (let [prev (peek cur)]
    (or
      ;; a combining mark, variation selector, ZWJ or skin tone binds leftwards
      (and (>= c 32) (zero? (char-width c)))
      ;; anything directly after a ZWJ is part of the same emoji sequence
      (= prev ZWJ)
      ;; two regional indicators make one flag; a third starts a new one
      (and (regional? c) (regional? prev)
           (odd? (count (filter regional? cur)))))))

(defn clusters
  "Split `s` into grapheme clusters — the units a terminal draws in one cell (or
  two). Never splits an emoji sequence, a flag or a base-plus-mark pair."
  [s]
  (cond
    (empty? s) []
    ;; one character, one cluster — no pair of them can join
    (plain-text? s) (mapv str s)
    :else
    (loop [cps (map int (seq s)) cur [] out []]
      (if-let [c (first cps)]
        (cond
          (empty? cur)          (recur (next cps) [c] out)
          (joins-previous? cur c) (recur (next cps) (conj cur c) out)
          :else                 (recur (next cps) [c] (conj out (apply str (map char cur)))))
        (if (seq cur)
          (conj out (apply str (map char cur)))
          out)))))

(defn cluster-width
  "Cells occupied by one grapheme cluster."
  [cluster]
  (let [cps (map int (seq cluster))]
    (cond
      ;; a flag is a pair of regional indicators, drawn as one double-wide glyph
      (>= (count (filter regional? cps)) 2) 2
      ;; VS16 asks for emoji presentation, which every terminal draws double-wide
      (some #(= VS16 %) cps) 2
      :else (reduce max 0 (map char-width cps)))))

(defn width
  "Cells occupied by string `s`."
  [s]
  (let [s (or s "")]
    (if (plain-text? s)
      (count s)
      (reduce + 0 (map cluster-width (clusters s))))))

(defn truncate
  "The longest prefix of `s` that fits in `cells` columns. A wide glyph that
  would straddle the edge is dropped rather than half-drawn, and a cluster is
  never cut in half."
  [s cells]
  (let [s (or s "")]
    (cond
      ;; a character is a cell, so the cut is at the index and never mid-cluster
      (plain-text? s) (if (<= (count s) cells) s (subs s 0 (max 0 cells)))
      (<= (width s) cells) s
      :else
      (loop [cs (clusters s) used 0 acc []]
        (if-let [c (first cs)]
          (let [w (cluster-width c)]
            (if (> (+ used w) cells)
              (apply str acc)
              (recur (next cs) (+ used w) (conj acc c))))
          (apply str acc))))))

(defn drop-cells
  "`s` with its first `cells` columns removed. A wide glyph straddling the cut is
  dropped whole, so the result starts on a cell boundary and is never wider than
  `(- (width s) cells)`. This is how a clipped screen paints a string whose left
  edge is off the visible area."
  [s cells]
  (let [s (or s "")]
    (cond
      (<= cells 0) s
      ;; a character is a cell, so nothing can straddle the cut
      (plain-text? s) (if (>= cells (count s)) "" (subs s cells))
      :else
      (loop [cs (clusters s) skipped 0]
        (cond
          (= skipped cells) (apply str cs)
          (empty? cs) ""
          :else
          (let [c (first cs)
                w (cluster-width c)]
            (if (<= (+ skipped w) cells)
              (recur (next cs) (+ skipped w))
              ;; this cluster straddles the cut: drop it and pad the gap it left
              (str (apply str (repeat (- (+ skipped w) cells) " "))
                   (apply str (next cs))))))))))

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
