(ns hypercrud.ui.result
  (:require-macros [hypercrud.ui.result :refer [-build-fiddle]])
  (:require
    [hyperfiddle.ui :refer [result field]]))


(def ^:export result hyperfiddle.ui/result)                 ; (tools.sources/code-search! "result/result")

(def ^:export fiddle (-build-fiddle))

(defn fiddle-xray [ctx class]
  (let [{:keys [:hypercrud.browser/fiddle
                #_:hypercrud.browser/result]} ctx]
    [:div {:class class}
     [:h3 (some-> @fiddle :fiddle/ident name)]
     (result ctx)
     (field [] ctx nil)]))
