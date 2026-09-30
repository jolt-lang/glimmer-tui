(ns glimmer-tui.keys
  "Key codes in, key events out.

  ncurses hands the event loop an integer: 263 for Backspace, 339 for Page Up,
  3 for ctrl-c, 106 for `j`. Comparing against those numbers spreads terminal
  trivia through every widget, so a code is decoded once, here, into a map:

    {:type :char :ch \\j :code 106}
    {:type :ctrl :ch \\u :code 21}
    {:type :page-up :code 339}

  and widgets and applications match against it by NAME:

    (keys/match? event \"ctrl+u\")
    (keys/match? event :page-up)
    (keys/match? event [\"end\" \"G\"])

  The vocabulary is the one every terminal program already uses — \"ctrl+d\",
  \"pgdn\", \"esc\", \"shift+tab\" — so a keymap can be written down in a prop, read
  by a person, and rendered into a help bar without a translation table."
  (:require [clojure.string :as str]))

;; ncurses key codes. Anything not listed decodes by its numeric range instead.
(def ^:private code->type
  {0   :ctrl-space
   9   :tab
   10  :enter
   13  :enter
   27  :escape
   32  :space
   127 :backspace
   258 :down
   259 :up
   260 :left
   261 :right
   262 :home
   263 :backspace
   330 :delete
   331 :insert
   338 :page-down
   339 :page-up
   343 :enter
   353 :back-tab
   360 :end
   409 :mouse
   410 :resize})

(defn decode
  "The key event for ncurses key `code`."
  [code]
  (let [t (code->type code)]
    (cond
      (= :ctrl-space t) {:type :ctrl :ch \space :code code}
      (= :space t)      {:type :space :ch \space :code code}
      t                 {:type t :code code}
      ;; C0 controls are ctrl-letters: 1 is ctrl-a, 26 is ctrl-z
      (and (>= code 1) (<= code 26)) {:type :ctrl :ch (char (+ 96 code)) :code code}
      ;; KEY_F(n) is KEY_F0 + n, and KEY_F0 is 264
      (and (>= code 265) (<= code 276)) {:type (keyword (str "f" (- code 264))) :code code}
      ;; printable, including everything above ASCII that a UTF-8 terminal sends
      (>= code 32) {:type :char :ch (char code) :code code}
      :else {:type :unknown :code code})))

;; --- UTF-8 -------------------------------------------------------------------
;; wgetch answers a byte at a time, and a terminal types anything past ASCII as
;; a multi-byte UTF-8 sequence: 한 is ED 95 9C. Decoding each byte as a
;; character of its own is mojibake, so the reader assembles a lead byte and the
;; continuations that follow into the one character they spell — the same
;; character wget_wch would have handed over whole. The decoding is pure and
;; lives here so both halves of input use it: the keystroke path (core.clj's
;; decode-input!) and the paste path.
(defn utf8-length
  "How many continuation bytes follow UTF-8 lead byte `b`, or nil when `b` is
  not one (ASCII, a continuation, or C0/C1 — the overlong forms that no real
  encoder emits, which exist only to smuggle NUL past a decoder)."
  [b]
  (cond
    (<= 0xC2 b 0xDF) 1
    (<= 0xE0 b 0xEF) 2
    (<= 0xF0 b 0xF4) 3
    :else nil))

(defn utf8-char
  "The character lead byte `lead` and continuation bytes `conts` spell, or nil
  when they do not spell a sequence: the wrong count, or a byte in `conts` that
  is not a continuation (0x80-0xBF)."
  [lead conts]
  (when-let [n (utf8-length lead)]
    (when (and (= n (count conts))
               (every? #(<= 0x80 % 0xBF) conts))
      (char (reduce (fn [cp b] (bit-or (bit-shift-left cp 6) (bit-and b 0x3F)))
                    (bit-and lead (dec (bit-shift-left 1 (- 7 n))))
                    conts)))))

(defn utf8-text
  "The text a run of bytes spells, decoded as UTF-8. A malformed sequence — a
  lead byte with no continuations behind it, a truncated one at the end of the
  run — keeps its bytes as the characters decode would have made of them, so
  whatever was actually typed arrives rather than nothing."
  [codes]
  (let [codes (vec codes)
        n (count codes)]
    (loop [i 0 out []]
      (if (>= i n)
        (apply str out)
        (let [b (nth codes i)]
          (if-let [len (and (>= b 0xC2) (<= b 0xF4) (utf8-length b))]
            (let [conts (subvec codes (inc i) (min n (+ i 1 len)))]
              (if-let [ch (utf8-char b conts)]
                (recur (inc (+ i len)) (conj out ch))
                (recur (inc i) (conj out (char b)))))
            (recur (inc i) (conj out (char b)))))))))

;; --- escape runs: bracketed paste and SGR mouse ------------------------------
;; Two things arrive as ESC and then bytes one at a time: a bracketed paste, which
;; wraps its text in ESC [ 200~ ... ESC [ 201~, and an SGR mouse report, which a
;; terminal in 1006 mode sends as ESC [ < b ; x ; y M (press/drag) or ... m
;; (release). Neither is a terminfo key, so ncurses hands the loop the bytes and
;; the loop recognises them here — which is why one function classifies both.
(def paste-start [91 50 48 48 126])             ; [200~
(def paste-end   [91 50 48 49 126])             ; [201~

(defn escape-run
  "What the codes read after an ESC have made so far: :paste-start, :paste-end,
  :mouse for a complete SGR mouse report, :partial while the run could still
  become one of those, or nil for a run that is none — which the event loop hands
  back as the alt chord and the keys it really was."
  [codes]
  (let [codes (vec codes)
        n (count codes)
        prefix-of? (fn [marker] (= codes (subvec marker 0 (min n (count marker)))))]
    (cond
      (= codes paste-start) :paste-start
      (= codes paste-end)   :paste-end
      (or (prefix-of? paste-start) (prefix-of? paste-end)) :partial
      ;; an SGR mouse report, ESC [ < ... M|m
      (= codes [91 60]) :partial
      (and (>= n 3) (= 91 (nth codes 0)) (= 60 (nth codes 1)))
      (let [last (nth codes (dec n))]
        (cond
          (or (= last 77) (= last 109)) :mouse   ; M or m
          (every? #(<= 48 % 59) (subvec codes 2)) :partial
          :else nil))
      :else nil)))

(defn- sgr-text
  "The bytes of an escape run as text, for parsing."
  [codes]
  (apply str (map char codes)))

(defn sgr-mouse
  "Parse an SGR mouse report, ESC [ < b ; x ; y M|m, into an event:

    {:type :mouse :button b :wheel w :x :y :action :press|:release :motion m}

  `b` carries the shift/alt/ctrl modifiers (4, 8, 16) and the motion flag (32) on
  top of the button; the modifiers are stripped, the motion flag is kept, so a
  drag is distinguishable from a press — an app deciding \"click, unless it
  turned into a drag\" needs both. 0-2 are left, middle and right; 64-67 is
  a wheel — :up, :down, :left or :right, with :button nil — and 128 and up are
  the extra buttons, kept as their raw code so they are never taken for a left
  click. x and y are 1-based on the wire and 0-based here. Returns nil for
  anything that is not a report."
  [codes]
  (when-let [[_ b x y end] (re-matches #"\[\<(\d+);(\d+);(\d+)([Mm])" (sgr-text codes))]
    (let [raw (parse-long b)
          base (bit-and raw (bit-not 60))
          wheel? (= 64 (bit-and base 192))]
      {:type :mouse
       :button (cond wheel? nil
                     (< base 64) (bit-and base 3)
                     :else base)
       :wheel (when wheel? ([:up :down :left :right] (bit-and base 3)))
       :x (dec (parse-long x))
       :y (dec (parse-long y))
       :action (if (= "M" end) :press :release)
       :motion (pos? (bit-and raw 32))})))

(defn paste
  "The event a decoded paste becomes. It is dispatched like a key — offered to
  the focused widget, then to its ancestors — so a widget that wants the text in
  one piece takes it, and one that does not is simply not typed into.

  `cut?` is true when the paste ended by the paste timeout rather than its end
  marker — a big paste over a slow link, stalled mid-stream longer than the
  timeout. The keys that follow are the rest of it, not typing."
  ([text] (paste text false))
  ([text cut?]
   (cond-> {:type :paste :text (or text "")}
     cut? (assoc :cut? true))))

(defn printable?
  "Whether `event` is a character a text field should insert."
  [event]
  (contains? #{:char :space} (:type event)))

(defn char-of
  "The character `event` carries, or nil."
  [event] (:ch event))

;; --- matching ----------------------------------------------------------------
(def ^:private aliases
  "Spellings people actually type, mapped onto event types."
  {"esc" :escape "escape" :escape
   "pgup" :page-up "pageup" :page-up "page-up" :page-up
   "pgdn" :page-down "pgdown" :page-down "pagedown" :page-down "page-down" :page-down
   "del" :delete "delete" :delete
   "ins" :insert "insert" :insert
   "bs" :backspace "backspace" :backspace
   "return" :enter "enter" :enter
   "spc" :space "space" :space
   "up" :up "down" :down "left" :left "right" :right
   "home" :home "end" :end "tab" :tab "mouse" :mouse "resize" :resize
   "paste" :paste
   "f1" :f1 "f2" :f2 "f3" :f3 "f4" :f4 "f5" :f5 "f6" :f6
   "f7" :f7 "f8" :f8 "f9" :f9 "f10" :f10 "f11" :f11 "f12" :f12})

(defn- parse
  "A binding string into {:ctrl :shift :alt :base}, where :base is either an
  event type or a one-character string."
  [s]
  (let [s (str s)]
    (if (= 1 (count s))
      {:base s}
      (let [parts (str/split s #"\+")
            mods (set (map str/lower-case (butlast parts)))
            base (last parts)]
        {:ctrl (contains? mods "ctrl")
         :shift (contains? mods "shift")
         :alt (contains? mods "alt")
         :base (or (aliases (str/lower-case base)) base)}))))

(defn- match-one? [event spec]
  (cond
    (nil? spec) false
    ;; a keyword names an event type directly: :page-up, :enter
    (keyword? spec) (= spec (:type event))
    (integer? spec) (= spec (:code event))
    :else
    (let [{:keys [ctrl shift alt base]} (parse spec)]
      (cond
        ;; shift+tab is the one shifted key a terminal reports distinctly
        (and shift (= :tab base)) (= :back-tab (:type event))
        ;; alt (meta) arrives as ESC then the key; the event loop pairs them up
        alt (and (= :alt (:type event))
                 (if (keyword? base)
                   (= base (:base-type event))
                   (= (str base) (str (:ch event)))))
        ctrl (and (= :ctrl (:type event))
                  (= (str/lower-case (str base))
                     (str/lower-case (str (:ch event)))))
        (keyword? base) (= base (:type event))
        ;; a bare character, matched case-sensitively: "g" and "G" differ
        :else (and (printable? event) (= (str base) (str (:ch event))))))))

(defn match?
  "Whether `event` matches `spec` — a binding string (\"ctrl+u\", \"pgdn\", \"G\"),
  an event-type keyword (:page-up), a raw key code, or a collection of any of
  those, in which case any one matching is enough."
  [event spec]
  (boolean
    (if (or (sequential? spec) (set? spec))
      (some #(match-one? event %) spec)
      (match-one? event spec))))

(defn matches-any?
  "Whether `event` matches any binding in `bindings` — a map of action -> spec,
  or a vector of [action spec] pairs. Returns the action, so a widget can
  dispatch on it."
  [event bindings]
  (some (fn [[action spec]] (when (match? event spec) action)) bindings))

(defn merge-bindings
  "`defaults` (a vector of [action spec] pairs) with `overrides` (a map of
  action -> spec) applied. Order is preserved, because a help bar reads these in
  the order they are declared; an override for an unknown action is appended."
  [defaults overrides]
  (if (empty? overrides)
    (vec defaults)
    (let [known (set (map first defaults))]
      (into (mapv (fn [[action spec]]
                    [action (if (contains? overrides action) (overrides action) spec)])
                  defaults)
            (remove (fn [[action _]] (contains? known action)) overrides)))))

;; --- describing --------------------------------------------------------------
(def ^:private type->label
  {:up "↑" :down "↓" :left "←" :right "→"
   :page-up "pgup" :page-down "pgdn" :escape "esc" :back-tab "shift+tab"
   :enter "enter" :tab "tab" :space "space" :backspace "backspace"
   :delete "del" :insert "ins" :home "home" :end "end"})

(defn describe
  "A short label for a binding, for a help bar: \"ctrl+u\", \"↑\", \"g\"."
  [spec]
  (cond
    (sequential? spec) (str/join "/" (map describe spec))
    (keyword? spec) (or (type->label spec) (name spec))
    (integer? spec) (describe (:type (decode spec)))
    :else
    (let [{:keys [ctrl shift alt base]} (parse spec)
          label (if (keyword? base) (or (type->label base) (name base)) (str base))]
      (str (when ctrl "ctrl+") (when alt "alt+") (when shift "shift+") label))))
