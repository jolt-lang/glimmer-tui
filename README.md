# glimmer-tui

The **terminal** backend for [glimmer](https://github.com/jolt-lang/glimmer), the
reactive GUI toolkit for [jolt](https://github.com/jolt-lang/jolt).

glimmer owns the portable half — reactive cells, the component model, the
reconciler — and knows nothing about any toolkit. This project supplies the other
half for a terminal: a widget set, box layout, painting through ncursesw, and an
input loop with keyboard focus, scrolling and mouse support. Requiring
`glimmer-tui.core` registers it, and the same components that render as GTK
widgets under [glimmer-gtk](https://github.com/jolt-lang/glimmer-gtk) render as
text.

```clojure
(ns myapp
  (:require [glimmer.ratom :as r :refer [atom]]
            [glimmer.core :as ui]
            [glimmer-tui.core :as tui]))

(defn counter []
  (let [count (atom 0)]
    (fn []
      [:vbox {:spacing 1 :margin 2}
       [:label {:label (str "Count: " @count) :bold true}]
       [:hbox {:spacing 2}
        [:button {:label "- 1" :on-click #(swap! count dec)}]
        [:button {:label "+ 1" :on-click #(swap! count inc)}]
        [:button {:label "quit" :on-click tui/quit!}]]])))

(defn -main [& _] (ui/run counter))
```

```
  Count: 2

  [ - 1 ]  [ + 1 ]  [ quit ]
```

Components, reactive state and reconciliation are documented in glimmer's README.
What follows is the terminal-specific part.

## Requirements

None to install, in the usual case. The backend binds **ncursesw** and uses only
the ncurses 6.0 API, which macOS ships in the dyld shared cache (6.0.20150808)
and every Linux distribution ships as `libncursesw.so.6`. On a minimal container
you may need `apt install libncursesw6`, since the base system only guarantees
`libtinfo6`.

Nothing newer is bound on purpose: the 6.1 extended-colour entry points are
absent from Apple's build. That is not a limit on what you can write — a `#ff6432`
prop works fine (see [Colour](#colour)) — only on how it reaches the terminal.

**jolt 0.7.24 or newer**, though, because of how jolt is linked rather than
anything either project does at runtime. Chez's expression editor links ncurses
into the jolt binary, and until 0.7.24 those symbols were exported: the
executable is searched before any library loaded through the FFI, so the ncurses
this backend binds had its own internal calls bound back into the kernel's older
copy. The result is `initscr` failing with "Error opening terminal" on a terminal
that works everywhere else, or a segfault inside it — see
[jolt#728](https://github.com/jolt-lang/jolt/pull/728). There is nothing to work
around here; the fix is the newer jolt.

## Running

```sh
jolt test      # the suite, headless: no terminal, tty or display needed
jolt counter   # the counter demo
jolt todo      # a task board: entry, checkbuttons, framed keyed list
jolt showcase  # every widget: list, table, scrolling text, dialog, gauges
jolt smoke     # non-interactive check against a real terminal
```

## Hiccup reference

Elements are `[:tag props? & children]`, as everywhere in glimmer. Strings and
numbers become labels, `nil` children are skipped, seqs are spliced.

**Containers**

| tag | holds | notes |
|---|---|---|
| `:window` | one child | the root |
| `:box` | many | `:orientation :horizontal\|:vertical` |
| `:hbox` / `:vbox` | many | the same box, orientation implied |
| `:frame` | one child | a border, with an optional `:label` set into the top edge |
| `:scroll` | one child | a viewport onto something bigger — see [Scrolling](#scrolling) |
| `:overlay` | one child | floats over the screen — see [Overlays](#overlays) |

**Widgets**

| tag | shows |
|---|---|
| `:label` | text; a newline makes it taller |
| `:button` | `[ label ]`, activated by Enter, Space or a click |
| `:entry` | a single-line text field |
| `:checkbutton` | `[x] label` |
| `:separator` | a rule along its `:orientation` |
| `:listbox` | a list with a cursor, scrolling itself to keep it visible |
| `:table` | columns sized to their contents, with a header and a row cursor |
| `:progress` | a progress bar |
| `:spinner` | one frame of an animation — see [Timers](#timers) |
| `:paginator` | `○●○` or `2/3` |
| `:help` | the bindings of whatever is focused — see [Keys and focus](#keys-and-focus) |

**Common props (every widget)**

- `:margin` / `:padding` — a number, `[vertical horizontal]`, or
  `[top right bottom left]`; or the per-edge `:margin-start`, `:padding-top`, …
- `:halign` / `:valign` — `:fill` (the default), `:start`, `:center`, `:end`
- `:hexpand` / `:vexpand` — boolean; expanders split the leftover space on that axis
- `:width-request` / `:height-request` — a floor on the natural size
- `:color` / `:bg` — see [Colour](#colour)
- `:bold`, `:dim`, `:underline`, `:reverse`, `:blink` — booleans
- `:sensitive false` — dims the widget and takes it out of the tab order
- `:autofocus true` — start with the focus here rather than on whatever is first

**Per-tag props**

- Label: `:label`/`:text`, `:align`
- Button: `:label`, `:brackets ["[ " " ]"]`
- Entry: `:text`, `:placeholder`, `:echo :normal|:password|:none`, `:echo-char`,
  `:char-limit`, `:width-request`, `:keys`, `:on-paste`
- Checkbutton: `:label`, `:active`, `:checked-mark`, `:unchecked-mark`
- Frame: `:label`, `:label-align`, `:border`, `:border-color`
- Separator: `:orientation`, `:char`
- Scroll: `:orientation :vertical|:horizontal|:both`, `:scrollbar`, `:keys`
- Overlay: `:anchor`, `:offset-x`, `:offset-y`, `:modal`, `:on-close`
- Listbox: `:items`, `:selected`, `:descriptions`, `:wrap`, `:cursor-prefix`,
  `:item-prefix`, `:keys`
- Table: `:columns`, `:rows`, `:selected`, `:header`, `:gap`, `:row-style`, `:keys`
- Progress: `:value` (0.0–1.0), `:bar`, `:bar-color`, `:show-percent`
- Spinner: `:tick`, `:frames`, `:label`
- Paginator: `:page`, `:total-pages` (or `:total-items` + `:per-page`), `:style`
- Help: `:bindings`, `:labels`, `:separator`, `:full`, `:ellipsis`

A `:columns` entry is `{:title "name" :key :name :width 12 :align :end
:style {:fg :green}}`; `:width`, `:align`, and `:style` are optional, and rows may
be maps or vectors. Table `:row-style` and column `:style` each accept a static
style map or a callback. The callback receives one context map and returns a
style map or `nil`.

A row callback receives `:row`, `:index`, `:selected?`, and `:focused?`. A cell
callback also receives `:column`, `:column-index`, and the original `:value`.
`:index` always refers to the complete `:rows` vector, including after scrolling.
Row keys such as `:color` have no built-in meaning; applications can explicitly
map such values to style attributes. Invalid style values and callback results
add no attributes.

Styles resolve in this order: common table style, row style, cell style, then the
selection modifier. A focused selected row adds `:reverse true`; an unfocused
selected row adds `:bold true`. Selection retains colors and other resolved
attributes.

**Events**

- `:on-click` — button activated. No args.
- `:on-change` — entry text changed. Receives the new text.
- `:on-activate` — Enter pressed in an entry, list or table. No args.
- `:on-toggled` — checkbutton activated. No args.
- `:on-select` — list or table cursor moved. Receives the index and the item.
- `:on-scroll` — a scroll container moved. Receives `{:x :y}`.
- `:on-close` — Esc pressed while a modal overlay is up. No args.
- `:on-key` — on a container: a key nothing inside it wanted. Receives the
  event; return truthy to consume it.

As in glimmer-gtk, a handler owns the state: `:on-toggled` flips the cell the
component reads, and `:active` comes back down as a prop. Widgets never toggle
themselves, and a list whose `:on-select` is ignored still works — it falls back
to its own cursor.

## Colour

A colour prop is any of

```clojure
:red  :bright-blue  :default     ; the sixteen ANSI names
208                              ; an index into the xterm 256-colour palette
"#ff6432"  "#f64"                ; a hex triple
[255 100 50]                     ; r/g/b
```

Everything is reduced to a palette index before ncurses sees it, and then folded
down again to what the terminal actually reports (`tigetnum "colors"`): the
closest entry in the 256-colour cube, then in the sixteen, then in the eight. So
a hex prop is not a portability decision — the UI keeps working on a 16-colour
tty, it just stops being exact. `:default` (and no colour at all) means the
terminal's own foreground or background, which is what keeps a glimmer UI
transparent over the user's theme instead of painting it black.

Frames take a border set: `:normal` (the default), `:rounded`, `:thick`,
`:double`, `:block`, `:outer-half-block`, `:inner-half-block`, `:ascii`,
`:hidden`, `:none`, or a map of the eight parts.

```clojure
[:frame {:label "results" :border :rounded :border-color "#5f87af" :padding 1}
 [:label {:label "…"}]]
```

## Keys and focus

| Key | Effect |
|---|---|
| `Tab` / `Shift-Tab` | move focus forward / back, wrapping |
| `Enter` / `Space` | activate the focused widget |
| arrows, `j`/`k`, `pgup`/`pgdn`, `ctrl-u`/`ctrl-d`, `g`/`G` | navigate a list, table or scroll |
| printable, `Backspace`, `Delete`, arrows, `Home`/`End` | edit the focused entry |
| `ctrl-a`/`ctrl-e`, `ctrl-w`, `ctrl-u`/`ctrl-k`, `alt-b`/`alt-f` | readline editing in an entry |
| a paste | inserted into the focused entry in one piece, never as keystrokes |
| `Esc` | close the topmost modal overlay |
| `ctrl-c` / `ctrl-q` | quit (configurable with `:quit-keys`) |
| mouse button 1 | focus and activate (or select the row) under the pointer |
| mouse wheel | scroll the container under the pointer |

Keys are named rather than numbered. `glimmer-tui.keys/decode` turns an ncurses
key code into an event — `{:type :page-up}`, `{:type :ctrl :ch \u}` — and
`match?` compares one against a binding written the way a person would say it:

```clojure
(k/match? event "ctrl+u")
(k/match? event :page-up)
(k/match? event ["end" "G"])
```

Every widget that answers to keys declares its bindings that way, so they can be
overridden per widget with `:keys`, and rendered:

```clojure
[:listbox {:items rows :keys {:down ["down" "n"]}}]
[:help {}]        ; renders whatever the focused widget answers to
```

The `:help` widget with no `:bindings` of its own reads them off the live tree,
which means the help bar cannot drift from what the keys actually do.

A key is offered to the focused widget first, then to each of its ancestors —
which is how `Page Down` reaches the scroll container a focused button happens to
be sitting in, and how an application binds a key of its own:

```clojure
[:vbox {:on-key (fn [e] (when (k/match? e "d") (open-dialog!) true))}
 ...]
```

Only what nobody wanted becomes a quit key, an `Esc` that closes a dialog, or an
`Enter` that presses a button. That ordering is deliberate: it means `q` can be a
quit key in an app that also has a text field, because the field sees it first.

Pasted text is not typing, and the backend does not pretend it is. The terminal
is put into bracketed-paste mode, so a paste arrives wrapped in `CSI 200~` …
`CSI 201~` and is decoded into a single event:

```clojure
{:type :paste :text "at foo\nat bar"}
```

It is dispatched like a key — the focused widget first, then its ancestors — so
an `:entry` inserts the whole thing at the caret, and a pasted newline is a
character rather than a Return that submits half a stack trace. A field that
wants to decide for itself takes `:on-paste`, which is given the text as pasted,
line breaks and all, instead of it being inserted:

```clojure
[:entry {:text @draft
         :on-change #(reset! draft %)
         ;; this app keeps the line breaks and shows the field a stand-in
         :on-paste (fn [text]
                     (reset! attachment text)
                     (reset! draft (str @draft "<pasted>")))}]
```

Without an `:on-paste` the control characters in a paste are flattened to
spaces, because a one-line field has nowhere to put a line break and ncurses
would act on it rather than draw it.

`:autofocus true` says which widget starts focused. It is worth more in a
terminal than it sounds: tree order gives the focus to whatever is highest on the
screen, usually a filter field, and a focused field swallows every letter — so an
application's single-key bindings would be dead until the user pressed Tab.

The focus ring is recomputed from the widget tree on every frame, in tree order,
so a component that renders a new button gets a sensible tab position with no
registration step. Focus follows the widget id rather than a position, which
means a keyed list can reorder around the focused row and you stay on the same
row. When the focused widget disappears, focus falls back to the first one left.

Focus lives in this project rather than in glimmer's core because it is a
property of how a toolkit is driven, not of the component model — under GTK,
focus comes from GTK.

## Scrolling

`:scroll` gives its child the full height the child asked for and shows a window
onto it. Nothing is re-measured while scrolling, so a thousand-row list costs
what a ten-row one does.

```clojure
[:scroll {:vexpand true}
 (into [:vbox {}] (for [line lines] [:label {:label line}]))]
```

A scroll takes keyboard focus only when nothing inside it can — a text viewport
is the thing you are driving, a scroll around a form is not. Either way its
bindings work, because a key the focused widget declines bubbles out to it, and
**moving focus scrolls the focused widget into view**: `Tab` into a field below
the fold brings it back on screen. Scrolling by hand does not drag the focus
along with it, so the wheel behaves the way a wheel should.

A scrollbar is drawn in the last column whenever there is more content than
viewport; `:scrollbar false` gives the column back.

Layout gives every node two sizes: `:natural`, what it would like, and `:min`,
what it can survive on. They are the same for most widgets — a label cannot be
shorter than its text — but a widget that handles its own overflow says so, and a
box short of room takes the shortfall from those first, in proportion to what
each has to give, before anything is clipped. That is what makes a scroll worth
having: without it the scroll's content height would come out of the box it sits
in and push the footer off the bottom, which is the problem it exists to solve.
`:listbox` and `:table` shrink the same way, down to a row (a table keeps its
header). A `:height-request` is a floor on both numbers, so asking for four rows
gets four rows even when space is short.

## Overlays

An `:overlay` is written where it belongs in the component that owns it, and laid
out against the *screen*: it takes no space at its declaration site and is not
clipped by the box it was declared in.

```clojure
(when @confirming?
  [:overlay {:anchor :center :on-close #(reset! confirming? false)}
   [:frame {:label "confirm" :border :double :padding 1}
    [:vbox {:spacing 1}
     [:label {:label "Delete the branch?"}]
     [:hbox {:spacing 2 :halign :center}
      [:button {:label "yes" :on-click delete!}]
      [:button {:label "no" :on-click #(reset! confirming? false)}]]]]])
```

While a modal overlay is up (the default) the focus ring is restricted to it, so
`Tab` cannot wander back into the page underneath, and `Esc` calls `:on-close`.
Overlays paint last, in declaration order, so a later one sits on top.

## Timers

Nothing animates itself. The loop wakes up every `:tick-ms` anyway, so a timer is
a due time and a thunk, and the thunk runs **on the loop thread** — the only
thread allowed to touch widgets:

```clojure
(let [tick (atom 0)]
  (tui/every! 80 #(swap! tick inc))
  (fn [] [:spinner {:tick @tick :label "fetching"}]))
```

`after!` fires once, `every!` repeats, `cancel!` stops one and the loop cancels
everything when it exits. A widget that ran its own timer would repaint whether
or not anything was looking at it, and would have to be told to stop.

## Testing a terminal UI without a terminal

Painting goes through `glimmer-tui.screen`, which is a map of functions with two
implementations: ncurses, and an in-memory grid. `glimmer-tui.core/attach!`
points a session at any screen, so a whole interactive app can be mounted,
painted, typed into and clicked in a plain unit test:

```clojure
(let [root (w/node :window {})
      screen (scr/buffer-screen 30 8)]
  (tui/attach! root screen)
  (ui/mount root :window [my-app])
  (tui/frame!)
  (tui/press! 9)        ; Tab
  (tui/press! 10)       ; Enter
  (tui/frame!)
  (scr/lines screen))   ; => ["count 1" "" "[ add ]" ...]
```

That is how this project's own suite works — the reconciler, layout, focus,
scrolling, overlays, entry editing and keyed reordering are all asserted on
rendered text, with no tty. `jolt smoke` covers the part that genuinely needs a
terminal.

`run-async` starts a UI on a background thread and hands back `{:quit! :result}`,
for driving one from a REPL. The REPL and the UI then share a terminal, so use
`tap>` rather than `println` while it is up: anything printed lands in the middle
of the frame.

## Text width

Widths are measured in **grapheme clusters**, not characters. `👍🏽` is one cluster
two cells wide rather than a thumb plus a stray skin tone, `🇯🇵` is one flag, and
`truncate` will never cut between the two. The table is a deliberate
approximation of wcwidth(3) — chosen over an FFI call in the layout path, and the
locale dependence that comes with it — with one departure: a cluster carrying
U+FE0F is two cells, because that is what a terminal draws even where wcwidth
reports one.

Text that is entirely printable ASCII skips all of that: it cannot hold a
combining mark, a wide glyph or an emoji, so its width is its length and a cut
is an index. That is worth measuring, because a repaint measures the same label
several times — during layout, again while clipping, again while aligning — and
on an English UI every one of those is this case. A full screen of text went
from 19ms a frame to 1.7ms, against a 30ms tick.

## Architecture

- **`glimmer-tui.ffi`** — ncursesw and libc bindings, key codes, attribute bits.
  No logic.
- **`glimmer-tui.text`** — grapheme clustering, display width, truncation.
- **`glimmer-tui.color`** — colour props to palette indices, and down to what the
  terminal has.
- **`glimmer-tui.border`** — the box-drawing sets.
- **`glimmer-tui.keys`** — key codes to named events, bracketed paste, SGR mouse
  reports, and matching against bindings.
- **`glimmer-tui.screen`** — the paint surface: ncurses, an in-memory grid, or
  either one clipped to a rectangle.
- **`glimmer-tui.widget`** — the widget registry and the node tree. Creating and
  patching widgets is pure data; nothing touches the terminal.
- **`glimmer-tui.widgets`** and the namespaces under it — the built-in widgets,
  each one a spec in that registry. A consumer adds its own the same way.
- **`glimmer-tui.layout`** — measure and arrange, pure functions over snapshots.
- **`glimmer-tui.render`** — paint a laid-out tree onto a screen, clipping each
  node to its parent and hoisting overlays to the top.
- **`glimmer-tui.curses`** — terminal lifecycle, colour pair allocation, the
  private terminal modes (paste, mouse), and input.
- **`glimmer-tui.core`** — the backend map, the event loop, focus, hit testing,
  scrolling and timers.

## Status

Beta. The widget set covers what a terminal application usually needs and the
reconciler, layout, focus, scrolling, overlays and input paths are covered by the
headless suite; the ncurses path is covered by `jolt smoke`.

Known limits. The wheel is read from SGR mouse reports (the terminal's 1000/1006
modes) rather than from ncurses, whose mouse ABI differs by build and, in the
version stock macOS ships, has no wheel-down at all. Turning the modes on is
unconditional, so a terminal that does not speak them simply gets no mouse — the
keyboard bindings are still the fallback. Colour is indexed, never 24-bit on the
wire.

On hostile environments. `usable-terminal?` checks the three conditions that
normally stop a UI from starting (no tty, no TERM, no terminfo entry), and
`ui/run` raises rather than proceeding when one of them fails. It cannot promise
more than that, because `initscr` reports failure by calling `exit()` rather than
returning — so a fourth condition nobody checked for looks exactly like a crash.

That is what the smoke step was hitting, and this README used to blame the CI
runner for it. It was not the runner. Every jolt binary before 0.7.24 exported
the ncurses its own kernel is linked against, which took priority over the one
this backend loads and left `initscr` unable to read a terminfo entry it should
have had no trouble with (see [Requirements](#requirements)). CI reported the
step as skipped because "Error opening terminal" is indistinguishable from a
machine that genuinely cannot host a UI. On a jolt with the fix, `jolt smoke`
passes on a real terminal — timers, scrolling, borders, wide glyphs and key
dispatch included.

## License

MIT (see `LICENSE`).
