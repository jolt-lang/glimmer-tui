(ns glimmer-tui.widgets
  "Installs the built-in widget set. Requiring this namespace registers every
  tag glimmer-tui.core documents; a consumer adding its own calls
  glimmer-tui.widget/register-widget! the same way these do."
  (:require [glimmer-tui.widgets.basic :as basic]
            [glimmer-tui.widgets.data :as data]
            [glimmer-tui.widgets.entry :as entry]
            [glimmer-tui.widgets.frame :as frame]
            [glimmer-tui.widgets.help :as help]
            [glimmer-tui.widgets.indicators :as indicators]
            [glimmer-tui.widgets.overlay :as overlay]
            [glimmer-tui.widgets.scroll :as scroll]))

(defn install!
  "Register every built-in widget. Idempotent."
  []
  (basic/install!)
  (frame/install!)
  (entry/install!)
  (scroll/install!)
  (data/install!)
  (indicators/install!)
  (help/install!)
  (overlay/install!)
  nil)

(defonce ^:private _installed (do (install!) true))
