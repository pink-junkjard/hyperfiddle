(ns hypercrud.ui.result
  (:require-macros [hypercrud.ui.result :refer [-build-fiddle]])
  (:require
    [hyperfiddle.ui :refer [#_result field]]))

(def ^:export fiddle (-build-fiddle))

(defn fiddle-xray [ctx class]
  (let [{:keys [:hypercrud.browser/fiddle
                #_:hypercrud.browser/result]} ctx]
    [:div {:class class}
     [:h3 (some-> @fiddle :fiddle/ident name)]
     (hyperfiddle.ui/result ctx)
     (field [] ctx nil)]))

(def ^:export result hyperfiddle.ui/result)                 ; (tools.sources/code-search! "result/result")
