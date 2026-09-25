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

    {:type :mouse :button b :wheel :up|:down|nil :x :y :action :press|:release}

  The button field carries the modifiers in its high bits and the button number
  in the low two; a wheel is button 64 (:up) or 65 (:down). x and y are 1-based on
  the wire and 0-based here. Returns nil for anything that is not a report."
  [codes]
  (when-let [[_ b x y end] (re-matches #"\[\<(\d+);(\d+);(\d+)([Mm])" (sgr-text codes))]
    (let [b (parse-long b)
          btn (bit-and b 3)]
      {:type :mouse
       :button (if (>= b 64) 0 btn)
       :wheel (cond (>= b 64) (if (zero? (bit-and b 1)) :up :down) :else nil)
       :x (dec (parse-long x))
       :y (dec (parse-long y))
       :action (if (= "M" end) :press :release)})))

(defn paste
  "The event a decoded paste becomes. It is dispatched like a key — offered to
  the focused widget, then to its ancestors — so a widget that wants the text in
  one piece takes it, and one that does not is simply not typed into."
  [text]
  {:type :paste :text (or text "")})

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
