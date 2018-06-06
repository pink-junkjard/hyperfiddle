(ns hypercrud.ui.result
  (:require-macros [hypercrud.ui.result :refer [-build-fiddle]])
  (:require [contrib.css :refer [classes]]
            [contrib.reagent :refer [fragment]]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hyperfiddle.ui :refer [markdown]]))


(def ^:export result hyperfiddle.ui/result)                 ; (tools.sources/code-search! "result/result")

(def ^:export fiddle (-build-fiddle))

(defn fiddle-xray [ctx class]
  (let [{:keys [:hypercrud.browser/fiddle
                #_:hypercrud.browser/result]} ctx]
    [:div {:class class}
     [:h3 (some-> @fiddle :fiddle/ident name)]
     (link-controls/anchors [] false ctx nil {:class "hyperfiddle-link-index"})
     (hyperfiddle.ui/result ctx)
     (link-controls/iframes [] false ctx)]))
