# glimmer-tui

The **terminal** backend for [glimmer](https://github.com/jolt-lang/glimmer), the
reactive GUI toolkit for [jolt](https://github.com/jolt-lang/jolt).

glimmer owns the portable half — reactive cells, the component model, the
reconciler — and knows nothing about any toolkit. This project supplies the other
half for a terminal: a widget set, box layout, painting through ncursesw, and an
input loop with keyboard focus and mouse support. Requiring `glimmer-tui.core`
registers it, and the same components that render as GTK widgets under
[glimmer-gtk](https://github.com/jolt-lang/glimmer-gtk) render as text.

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
absent from Apple's build, and a terminal UI does not need colour pairs past 256.
The point of a terminal backend is running on a machine you did not provision.

## Running

```sh
jolt test      # the suite, headless: no terminal, tty or display needed
jolt counter   # the counter demo
jolt todo      # a task board: entry, checkbuttons, framed keyed list
jolt smoke     # non-interactive check against a real terminal
```

## Hiccup reference

Elements are `[:tag props? & children]`, as everywhere in glimmer. Strings and
numbers become labels, `nil` children are skipped, seqs are spliced.

**Containers:** `:window` (the root, single child), `:box`
(`:orientation :horizontal|:vertical`), `:hbox`, `:vbox`, `:frame` (single child,
box-drawing border with an optional `:label` in the top edge).

**Leaf widgets:** `:label`, `:button`, `:entry`, `:checkbutton`, `:separator`.

**Common props (every widget):**

- `:margin`, or `:margin-start`/`:margin-end`/`:margin-top`/`:margin-bottom`
- `:halign`/`:valign` — `:fill` (the default), `:start`, `:center`, `:end`
- `:hexpand`/`:vexpand` — boolean; expanders split the leftover space on that axis
- `:width-request`/`:height-request` — a floor on the natural size
- `:color`/`:bg` — `:black :red :green :yellow :blue :magenta :cyan :white :default`
- `:bold`, `:dim`, `:underline`, `:reverse` — booleans
- `:sensitive false` — dims the widget and takes it out of the tab order

**Per-tag props:**

- Label: `:label`/`:text` (a newline makes it taller)
- Button: `:label`
- Entry: `:text`, `:placeholder`, `:width-request`
- Checkbutton: `:label`, `:active`
- Frame: `:label`
- Separator: `:orientation`

**Events:**

- `:on-click` — button activated. No args.
- `:on-change` — entry text changed. Receives the new text.
- `:on-activate` — Enter pressed in an entry. No args.
- `:on-toggled` — checkbutton activated. No args.

As in glimmer-gtk, a handler owns the state: `:on-toggled` flips the cell the
component reads, and `:active` comes back down as a prop. Widgets never toggle
themselves.

## Keys and focus

| Key | Effect |
|---|---|
| `Tab` / `Shift-Tab` | move focus forward / back, wrapping |
| `Enter` / `Space` | activate the focused widget |
| printable, `Backspace`, `Delete`, arrows, `Home`, `End` | edit the focused entry |
| `ctrl-c` / `ctrl-q` | quit (configurable with `:quit-keys`) |
| mouse button 1 | focus and activate whatever is under the pointer |

The focused widget is drawn in reverse video, and the terminal cursor is parked
in the focused entry and hidden otherwise.

The focus ring is recomputed from the widget tree on every frame, in tree order,
so a component that renders a new button gets a sensible tab position with no
registration step. Focus follows the widget id rather than a position, which
means a keyed list can reorder around the focused row and you stay on the same
row. When the focused widget disappears, focus falls back to the first one left.

Focus lives in this project rather than in glimmer's core because it is a
property of how a toolkit is driven, not of the component model — under GTK,
focus comes from GTK.

`ui/run` takes `:tick-ms` (input poll interval, default 30, which is also the
worst-case delay before work posted from another thread is picked up),
`:quit-keys`, and `:auto-quit-ms` for smoke tests.

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
entry editing and keyed reordering are all asserted on rendered text, with no
tty. `jolt smoke` covers the part that genuinely needs a terminal.

## Architecture

- **`glimmer-tui.ffi`** — ncursesw and libc bindings, key codes, attribute bits,
  the MEVENT layout. No logic.
- **`glimmer-tui.text`** — display width in cells, and truncation built on it, so
  CJK and emoji do not shift a row.
- **`glimmer-tui.screen`** — the paint surface: ncurses or an in-memory grid.
- **`glimmer-tui.widget`** — the widget registry and the node tree. Creating and
  patching widgets is pure data; nothing touches the terminal.
- **`glimmer-tui.layout`** — measure and arrange, pure functions over snapshots.
- **`glimmer-tui.render`** — paint a laid-out tree onto a screen, clipping each
  node to its parent.
- **`glimmer-tui.curses`** — terminal lifecycle, colour pair allocation, input.
- **`glimmer-tui.core`** — the backend map, the event loop, focus and hit testing.

## Status

Early. The widget set is small and there is no scrolling viewport yet, so a UI
taller than the terminal is clipped rather than scrolled. Colour is the 8-colour
indexed palette. The reconciler, layout, focus and input paths are covered by the
headless suite; the ncurses path is covered by `jolt smoke`.

Two things to know about hostile environments. `usable-terminal?` checks the
three conditions that normally stop a UI from starting (no tty, no TERM, no
terminfo entry), and `ui/run` raises rather than proceeding when one of them
fails. It cannot promise more than that: `initscr` reports failure by calling
`exit()`, and a GitHub Actions runner manages to fail it even with a pty on all
three descriptors, a terminfo entry present and a window size set. That is why CI
runs the headless suite and reports the smoke step as skipped there.
