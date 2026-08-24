(ns glimmer-tui.showcase
  "Everything in the widget set, in one screen.

  A list on the left picks a page; the page on the right is a scrolling text
  viewport, a table, or a progress panel. `d` opens a modal dialog, the help bar
  along the bottom is generated from whatever is focused, and a timer drives the
  spinner and the progress bar without anyone pressing a key.

  Run: jolt showcase.  Tab moves, arrows/j/k navigate, d for a dialog, q quits."
  (:require [glimmer.core :as ui]
            [glimmer.ratom :as r :refer [atom reaction]]
            [glimmer-tui.core :as tui]
            [glimmer-tui.keys :as k]))

(def ^:private prose
  (str "glimmer-tui paints a reactive component tree into a terminal.\n"
       "\n"
       "This pane is a :scroll — the text inside it is laid out at its full\n"
       "height and the viewport shows a window onto it. Nothing is re-measured\n"
       "while you scroll, so a long document costs the same as a short one.\n"
       "\n"
       "Bindings come from the widget, not from this comment:\n"
       "  j / k or the arrows   one line\n"
       "  ctrl-d / ctrl-u       half a page\n"
       "  pgdn / pgup           a page\n"
       "  g / G                 the ends\n"
       "  the mouse wheel       three lines\n"
       "\n"
       "The bar at the bottom of the screen is a :help widget with no bindings\n"
       "of its own. It renders whatever the focused widget answers to, read off\n"
       "the live tree, so it cannot drift from what the keys actually do.\n"
       "\n"
       "Widths are measured in grapheme clusters rather than characters, which\n"
       "is why these line up:\n"
       "  ascii     |x|\n"
       "  CJK       |日|\n"
       "  emoji     |👍🏽|\n"
       "  flag      |🇯🇵|\n"
       "\n"
       "Colour is indexed and folded down to whatever the terminal reports, so\n"
       "a hex prop works on an eight-colour tty — it just stops being exact.\n"
       "\n"
       "Scroll to the end to see that the scrollbar thumb agrees with you.\n"
       "\n"
       "· fin ·"))

(def ^:private rows
  [{:name "text"    :kind "viewport" :lines 24}
   {:name "table"   :kind "grid"     :lines 4}
   {:name "gauges"  :kind "panel"    :lines 3}
   {:name "dialogs" :kind "overlay"  :lines 1}
   {:name "colours" :kind "palette"  :lines 8}])

(defn- text-page []
  [:scroll {:vexpand true :hexpand true}
   [:label {:label prose}]])

(defn- table-page []
  [:vbox {:spacing 1 :vexpand true}
   [:label {:label "A table sizes its columns to their contents." :dim true}]
   [:table {:columns [{:title "widget" :key :name}
                      {:title "kind" :key :kind}
                      {:title "lines" :key :lines :align :end}]
            :rows rows
            :vexpand true}]])

(defn- gauges-page [tick]
  (let [pct (/ (mod @tick 100) 100.0)]
    [:vbox {:spacing 1 :vexpand true}
     [:hbox {:spacing 2}
      [:spinner {:tick @tick :label "working" :color :cyan}]
      [:label {:label (str (int (* 100 pct)) "%")}]]
     [:progress {:value pct :bar :blocks :bar-color :green :show-percent true
                 :width-request 24 :halign :start}]
     [:progress {:value (- 1 pct) :bar :brackets :width-request 24 :halign :start}]
     [:paginator {:page (mod (quot @tick 20) 4) :total-pages 4}]]))

(defn- palette-page []
  [:vbox {:spacing 0 :vexpand true}
   [:label {:label "the sixteen names" :dim true}]
   (into [:hbox {:spacing 1}]
         (for [c [:red :green :yellow :blue :magenta :cyan :white]]
           [:label {:label (name c) :color c}]))
   [:label {:label "a hex prop, folded into whatever this terminal has" :dim true}]
   (into [:hbox {:spacing 1}]
         (for [h ["#ff6432" "#32ff64" "#6432ff" "#ffcc00"]]
           [:label {:label "████" :color h}]))
   [:label {:label "border sets" :dim true}]
   (into [:hbox {:spacing 1}]
         (for [b [:normal :rounded :thick :double :ascii]]
           [:frame {:border b} [:label {:label (name b)}]]))])

(defn showcase []
  (let [page (atom 0)
        tick (atom 0)
        dialog? (atom false)
        answer (atom "—")
        current (reaction (:name (nth rows @page)))]
    (tui/every! 120 #(swap! tick inc))
    (fn []
      ;; :on-key on a container is how an application binds a key of its own:
      ;; it sees whatever the focused widget did not want.
      [:vbox {:vexpand true :hexpand true
              :on-key (fn [event]
                        (when (k/match? event "d")
                          (reset! dialog? true)
                          true))}
       [:label {:label " glimmer-tui showcase " :bold true :reverse true}]

       [:hbox {:spacing 1 :vexpand true :margin-top 1}
        [:frame {:label "pages" :border :rounded :valign :fill}
         [:listbox {:items (mapv :name rows)
                    :selected @page
                    :on-select (fn [i _] (reset! page i))
                    :vexpand true}]]

        [:frame {:label @current :border :rounded :hexpand true :vexpand true
                 :padding [0 1]}
         (case @page
           0 [text-page]
           1 [table-page]
           2 [gauges-page tick]
           3 [:vbox {:spacing 1 :vexpand true}
              [:label {:label (str "last answer: " @answer)}]
              [:button {:label "open a dialog" :on-click #(reset! dialog? true)}]]
           4 [palette-page])]]

       [:separator {}]
       [:hbox {:spacing 2}
        [:help {:hexpand true}]
        [:label {:label "d dialog  q quit" :dim true}]]

       (when @dialog?
         [:overlay {:anchor :center :on-close #(reset! dialog? false)}
          [:frame {:label "confirm" :border :double :padding 1}
           [:vbox {:spacing 1}
            [:label {:label "An overlay floats over the page"}]
            [:label {:label "and keeps the focus to itself." :dim true}]
            [:hbox {:spacing 2 :halign :center}
             [:button {:label "yes" :on-click #(do (reset! answer "yes")
                                                   (reset! dialog? false))}]
             [:button {:label "no" :on-click #(do (reset! answer "no")
                                                  (reset! dialog? false))}]]
            [:label {:label "esc also closes it" :dim true :halign :center}]]]])])))

(defn -main [& _]
  (ui/run showcase :quit-keys #{"ctrl+c" "ctrl+q" "q"}))
