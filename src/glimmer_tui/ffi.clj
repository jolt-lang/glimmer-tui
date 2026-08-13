(ns glimmer-tui.ffi
  "Raw C bindings for ncursesw, plus the two libc calls a terminal app needs.
  A thin defcfn layer — no logic. The screen implementation in
  glimmer-tui.curses is built on top of these.

  Only the ncurses 6.0 subset is bound, deliberately. macOS ships ncurses
  6.0.20150808 in the dyld shared cache, so binding nothing newer means the
  backend runs with no install at all on macOS and on every Linux that has
  libncursesw. The 6.1 extended-colour entry points (init_extended_pair,
  alloc_pair) are NOT bound: they are absent from Apple's build, and binding
  them would trade universal availability for colour pairs past 256, which a
  terminal UI does not need.

  Two ABI details worth knowing, both verified against the ncurses headers:

    attr_t / chtype is `unsigned int` (32 bits) on macOS and Linux alike — the
    LP64 `unsigned long` variant in the header sits behind a disabled `#if 0`.
    So attributes marshal as :uint.

    MEVENT is { short id; int x, y, z; mmask_t bstate; } with mmask_t an
    unsigned long, giving field offsets 0/4/8/12/16 and a 24-byte struct on
    both platforms. glimmer-tui.curses reads x, y and bstate at those offsets.

  Mouse button masks differ between NCURSES_MOUSE_VERSION 1 (macOS) and 2
  (Linux): the per-button shift is 6 bits versus 5. The BUTTON1 bits are
  identical in both (released 1, pressed 2, clicked 4), which is why this
  backend only interprets button 1."
  (:require [jolt.ffi :as ffi]))

;; --- libc --------------------------------------------------------------------
;; A UTF-8 ctype locale must be set before initscr, or ncursesw renders
;; multibyte text as question marks.
;;
;; The LC_* constants differ between platforms: on macOS LC_ALL is 0 and
;; LC_CTYPE is 2; on glibc LC_CTYPE is 0 and LC_ALL is 6. Rather than detect the
;; platform, glimmer-tui.curses calls setlocale with BOTH 0 and 6 — category 0
;; is LC_ALL on macOS and LC_CTYPE on Linux, so one call does the necessary work
;; on either, and the other is a harmless no-op that returns NULL.
;; The return value is the resulting locale name, or NULL for an unknown
;; category — read as :pointer so a NULL return is just a null pointer rather
;; than an attempt to marshal one into a string.
(ffi/defcfn setlocale "setlocale" [:int :string] :pointer)

;; Whether a file descriptor is a terminal. initscr on a pipe prints
;; "Error opening terminal" and exits the PROCESS, taking the REPL with it, so
;; glimmer-tui.curses checks first and throws something catchable instead.
(ffi/defcfn isatty "isatty" [:int] :int)

;; --- lifecycle ---------------------------------------------------------------
;; initscr returns stdscr, which is the only window this backend draws into:
;; layout is computed in jolt, so ncurses windows would buy nothing over one
;; full-screen buffer, and a single window keeps the diffing in doupdate.
(ffi/defcfn initscr  "initscr"  [] :pointer)
(ffi/defcfn endwin   "endwin"   [] :int)
(ffi/defcfn isendwin "isendwin" [] :int)
;; raw rather than cbreak: raw also turns off ISIG, so ctrl-c arrives as key 3
;; instead of a SIGINT that would kill the process with the terminal still in
;; raw mode. The loop can then quit through its normal path and restore the tty.
(ffi/defcfn raw      "raw"      [] :int)
(ffi/defcfn cbreak   "cbreak"   [] :int)
(ffi/defcfn noecho   "noecho"   [] :int)
(ffi/defcfn nonl     "nonl"     [] :int)
(ffi/defcfn curs-set "curs_set" [:int] :int)
(ffi/defcfn flushinp "flushinp" [] :int)

;; --- window state ------------------------------------------------------------
(ffi/defcfn keypad   "keypad"   [:pointer :int] :int)
(ffi/defcfn notimeout "notimeout" [:pointer :int] :int)
;; wtimeout sets how long wgetch blocks: the event loop uses a short timeout so
;; it can also drain work posted from other threads and notice a resize.
(ffi/defcfn wtimeout "wtimeout" [:pointer :int] :void)
(ffi/defcfn wgetch   "wgetch"   [:pointer] :int :blocking)
(ffi/defcfn getmaxx  "getmaxx"  [:pointer] :int)
(ffi/defcfn getmaxy  "getmaxy"  [:pointer] :int)
(ffi/defcfn leaveok  "leaveok"  [:pointer :int] :int)

;; --- drawing -----------------------------------------------------------------
(ffi/defcfn werase      "werase"      [:pointer] :int)
(ffi/defcfn wmove       "wmove"       [:pointer :int :int] :int)
;; waddnstr takes a byte count, and ncursesw decodes those bytes as UTF-8 once a
;; ctype locale is set — so painting never has to build cchar_t arrays.
(ffi/defcfn waddnstr    "waddnstr"    [:pointer :string :int] :int)
(ffi/defcfn mvwaddnstr  "mvwaddnstr"  [:pointer :int :int :string :int] :int)
;; wattrset takes attributes and the colour pair packed into one chtype: the
;; pair number occupies bits 8..15 (COLOR_PAIR(n) is n << NCURSES_ATTR_SHIFT).
(ffi/defcfn wattrset    "wattrset"    [:pointer :int] :int)
(ffi/defcfn wbkgd       "wbkgd"       [:pointer :uint] :int)
(ffi/defcfn mvwhline    "mvwhline"    [:pointer :int :int :uint :int] :int)
(ffi/defcfn mvwvline    "mvwvline"    [:pointer :int :int :uint :int] :int)
(ffi/defcfn wnoutrefresh "wnoutrefresh" [:pointer] :int)
(ffi/defcfn doupdate    "doupdate"    [] :int)

;; --- colour ------------------------------------------------------------------
(ffi/defcfn has-colors         "has_colors"         [] :int)
(ffi/defcfn start-color        "start_color"        [] :int)
;; Maps colour -1 onto the terminal's own default foreground/background, which is
;; what keeps a glimmer UI transparent over the user's colour scheme.
(ffi/defcfn use-default-colors "use_default_colors" [] :int)
(ffi/defcfn init-pair          "init_pair"          [:int :int :int] :int)

;; --- mouse -------------------------------------------------------------------
(ffi/defcfn mousemask     "mousemask"     [:ulong :pointer] :ulong)
(ffi/defcfn mouseinterval "mouseinterval" [:int] :int)
(ffi/defcfn getmouse      "getmouse"      [:pointer] :int)

;; --- constants ---------------------------------------------------------------
;; Attributes: NCURSES_BITS(1U, n) is 1 << (n + 8).
(def A-NORMAL    0)
(def A-STANDOUT  (bit-shift-left 1 16))
(def A-UNDERLINE (bit-shift-left 1 17))
(def A-REVERSE   (bit-shift-left 1 18))
(def A-BLINK     (bit-shift-left 1 19))
(def A-DIM       (bit-shift-left 1 20))
(def A-BOLD      (bit-shift-left 1 21))

(defn color-pair
  "Pack colour pair `n` into the chtype attribute word (COLOR_PAIR(n))."
  [n] (bit-shift-left n 8))

;; The eight terminfo colours, by ncurses index. -1 means \"whatever the terminal
;; was already using\", enabled by use_default_colors.
(def COLOR-DEFAULT -1)
(def COLOR-BLACK 0)
(def COLOR-RED 1)
(def COLOR-GREEN 2)
(def COLOR-YELLOW 3)
(def COLOR-BLUE 4)
(def COLOR-MAGENTA 5)
(def COLOR-CYAN 6)
(def COLOR-WHITE 7)

;; Key codes (octal in the header, decimal here).
(def KEY-DOWN 258)
(def KEY-UP 259)
(def KEY-LEFT 260)
(def KEY-RIGHT 261)
(def KEY-HOME 262)
(def KEY-BACKSPACE 263)
(def KEY-DC 330)
(def KEY-NPAGE 338)
(def KEY-PPAGE 339)
(def KEY-ENTER 343)
(def KEY-BTAB 353)
(def KEY-END 360)
(def KEY-MOUSE 409)
(def KEY-RESIZE 410)
(def ERR -1)

;; ALL_MOUSE_EVENTS is REPORT_MOUSE_POSITION-1 in both mouse ABI versions; this
;; value covers every button event either version can report.
(def ALL-MOUSE-EVENTS 0x7ffffff)
;; BUTTON1 released/pressed/clicked — the same three bits in mouse version 1 and 2.
(def BUTTON1-RELEASED 1)
(def BUTTON1-PRESSED  2)
(def BUTTON1-CLICKED  4)

;; MEVENT field offsets (see the namespace docstring).
(def MEVENT-SIZE 24)
(def MEVENT-X-OFFSET 4)
(def MEVENT-Y-OFFSET 8)
(def MEVENT-BSTATE-OFFSET 16)
