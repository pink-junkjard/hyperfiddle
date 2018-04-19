(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require [contrib.data :refer [kwargs unwrap]]
            [contrib.reagent :refer [fragment]]
            [hypercrud.ui.control.markdown-rendered :refer [markdown]]
            [hypercrud.ui.tooltip :refer [tooltip]]
            [hyperfiddle.ide.fiddles.topnav :refer [shadow-fiddle hijack-renderer loading-spinner]]))


(defn fiddle-src-renderer [ctx]
  (let [#_#_ctx (shadow-fiddle ctx)
        rtype (:fiddle/type @(:hypercrud.browser/result ctx))]
    [:div
     ((:cell ctx) [true 0 :fiddle/type] ctx)
     (case rtype
       :entity ((:cell ctx) [true 0 :fiddle/pull] ctx)
       :query ((:cell ctx) [true 0 :fiddle/query] ctx)
       nil)
     ((:browse ctx) :ui [] ctx (partial hijack-renderer false))
     ((:browse ctx) :links [] ctx (partial hijack-renderer true))]))
