(ns glimmer-tui.color
  "Colour values, and how they are reduced to what the terminal in front of you
  can actually show.

  A colour prop is one of

    :red, :bright-blue, …   one of the sixteen ANSI names
    :default                the terminal's own foreground/background
    0 – 255                 an index into the xterm 256-colour palette
    \"#ff6432\"               a hex triple (\"#f64\" also works)
    [255 100 50]            an r/g/b triple

  and everything is normalised to a palette INDEX before it reaches ncurses. The
  reason is portability, which is the same reason this backend binds only the
  ncurses 6.0 API: `init_pair` with an index below `COLORS` is 6.0, while the
  direct-RGB entry points (`init_extended_pair`, `alloc_pair`) are 6.1 and are
  missing from the ncurses macOS ships. Reducing 24-bit colour to the 256-colour
  cube here means a hex prop works on a library that has never heard of it.

  `downgrade` then folds the index down again for terminals that report fewer
  colours, so the same UI is legible on a 16-colour tty and an 8-colour one."
  (:require [clojure.string :as str]))

;; --- names -------------------------------------------------------------------
(def names
  "The sixteen ANSI colour names and their palette indices."
  {:black 0   :red 1          :green 2         :yellow 3
   :blue 4    :magenta 5      :cyan 6          :white 7
   :bright-black 8            :bright-red 9    :bright-green 10
   :bright-yellow 11          :bright-blue 12  :bright-magenta 13
   :bright-cyan 14            :bright-white 15})

;; Aliases for the way the eight dim colours are usually spoken about.
(def ^:private aliases {:grey 8 :gray 8 :bright-grey 7 :bright-gray 7})

(def default
  "The terminal's own default colour. ncurses spells it -1, which
  use_default_colors turns into \"leave this alone\"."
  -1)

;; --- parsing -----------------------------------------------------------------
(defn- hex-digit [c]
  (let [n (int c)]
    (cond
      (and (>= n 48) (<= n 57)) (- n 48)          ; 0-9
      (and (>= n 97) (<= n 102)) (- n 87)         ; a-f
      (and (>= n 65) (<= n 70)) (- n 55)          ; A-F
      :else nil)))

(defn parse-hex
  "\"#ff6432\" or \"#f64\" as [r g b], or nil when it is not a hex colour."
  [s]
  (let [s (str/replace (str s) #"^#" "")
        ds (map hex-digit s)]
    (when (every? some? ds)
      (case (count ds)
        3 (mapv (fn [d] (+ (* 16 d) d)) ds)
        6 (mapv (fn [[hi lo]] (+ (* 16 hi) lo)) (partition 2 ds))
        nil))))

;; --- 24-bit to the 256-colour cube -------------------------------------------
;; The xterm palette above 15 is a 6x6x6 colour cube (16-231) followed by a
;; 24-step grey ramp (232-255). A grey is much better served by the ramp, whose
;; steps are ~10 apart, than by the cube, whose greys are ~40 apart — so grey is
;; tried first.
(def ^:private cube-levels [0 95 135 175 215 255])

(defn- nearest-cube-index [v]
  (first (apply min-key second (map-indexed (fn [i l] [i (abs (- v l))]) cube-levels))))

(defn rgb->ansi256
  "The closest xterm-256 palette index to the 24-bit colour [r g b]."
  [[r g b]]
  (let [grey? (and (< (abs (- r g)) 12) (< (abs (- g b)) 12) (< (abs (- r b)) 12))
        grey-step (max 0 (min 23 (Math/round (double (/ (- (/ (+ r g b) 3.0) 8) 10)))))
        grey-index (+ 232 grey-step)
        grey-value (+ 8 (* 10 grey-step))
        cube [(nearest-cube-index r) (nearest-cube-index g) (nearest-cube-index b)]
        cube-index (+ 16 (* 36 (cube 0)) (* 6 (cube 1)) (cube 2))
        cube-value (mapv cube-levels cube)
        dist (fn [[cr cg cb]] (+ (* (- r cr) (- r cr)) (* (- g cg) (- g cg)) (* (- b cb) (- b cb))))]
    (if (and grey? (<= (dist [grey-value grey-value grey-value]) (dist cube-value)))
      grey-index
      cube-index)))

;; --- normalising a prop ------------------------------------------------------
(defn index
  "The palette index for a colour prop, or `default` (-1) when it is nil, unknown
  or explicitly :default."
  [c]
  (cond
    (nil? c) default
    (= :default c) default
    (keyword? c) (or (names c) (aliases c) default)
    (integer? c) (if (and (>= c 0) (<= c 255)) c default)
    (string? c) (if-let [rgb (parse-hex c)] (rgb->ansi256 rgb) default)
    (and (sequential? c) (= 3 (count c))) (rgb->ansi256 (vec c))
    :else default))

;; --- profiles ----------------------------------------------------------------
(defn profile
  "The colour profile implied by a terminal that reports `n` colours:
  :true-color is never claimed, because nothing here emits 24-bit escapes —
  what matters is how far an index has to be folded down."
  [n]
  (cond
    (>= n 256) :ansi256
    (>= n 16) :ansi16
    (>= n 8) :ansi8
    :else :mono))

(defn env-profile
  "The profile the environment advertises, for code that wants to know before a
  terminal exists. COLORTERM is what a truecolor terminal sets; TERM carrying
  \"256color\" is the usual 256 signal."
  []
  (let [term (or (System/getenv "TERM") "")
        colorterm (or (System/getenv "COLORTERM") "")]
    (cond
      (contains? #{"truecolor" "24bit"} colorterm) :ansi256
      (str/includes? term "256color") :ansi256
      (or (= "" term) (= "dumb" term)) :mono
      :else :ansi16)))

;; The 256-palette's own values, needed to fold an index back down to 16 or 8.
(defn- index->rgb [i]
  (cond
    (< i 16) (let [base (if (< i 8) 0 128)
                   bright (if (< i 8) 128 255)
                   bit (fn [n] (if (pos? (bit-and i n)) bright base))]
               ;; the classic ANSI palette: bit 0 red, bit 1 green, bit 2 blue
               (if (= i 7) [192 192 192]
                   (if (= i 8) [128 128 128]
                       [(bit 1) (bit 2) (bit 4)])))
    (< i 232) (let [i (- i 16)]
                [(cube-levels (quot i 36))
                 (cube-levels (quot (mod i 36) 6))
                 (cube-levels (mod i 6))])
    :else (let [v (+ 8 (* 10 (- i 232)))] [v v v])))

(defn downgrade
  "Fold palette index `i` into what `profile` can show. An index a terminal does
  not have is not an error, it is a colour that has to be approximated: the
  256-cube entry closest in RGB terms, then the sixteen, then the eight."
  [i profile]
  (cond
    (neg? i) i
    (= :ansi256 profile) i
    (= :mono profile) default
    :else
    (let [limit (if (= :ansi8 profile) 8 16)]
      (if (< i limit)
        i
        (let [rgb (index->rgb i)
              dist (fn [j] (let [[r g b] (index->rgb j)
                                 [ar ag ab] rgb]
                             (+ (* (- r ar) (- r ar))
                                (* (- g ag) (- g ag))
                                (* (- b ab) (- b ab)))))]
          (apply min-key dist (range limit)))))))

(defn resolve-color
  "A colour prop, all the way to an index this terminal can use."
  [c profile]
  (downgrade (index c) profile))
