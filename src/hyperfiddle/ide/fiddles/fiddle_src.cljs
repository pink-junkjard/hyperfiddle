(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require [contrib.data :refer [kwargs unwrap]]
            [contrib.reagent :refer [fragment]]
            [hypercrud.ui.control.markdown-rendered :refer [markdown]]
            [hypercrud.ui.tooltip :refer [tooltip]]
            [hyperfiddle.ide.fiddles.topnav :refer [shadow-fiddle hijack-renderer loading-spinner]]))


(defn fiddle-src-renderer [ctx]
  (let [#_#_ctx (shadow-fiddle ctx)]
    [:div
     ((:cell ctx) [true 0 :fiddle/type] ctx)
     ((:cell ctx) [true 0 :fiddle/pull] ctx)
     ((:cell ctx) [true 0 :fiddle/query] ctx)
     ((:browse ctx) :ui [] ctx (partial hijack-renderer false))
     ((:browse ctx) :links [] ctx (partial hijack-renderer true))]))

(comment
  (let [rtype (-> ctx :cell-data deref :fiddle/type)
        visible (case (:attribute field)
                  :fiddle/ident false
                  :fiddle/query (= rtype :query)
                  :fiddle/pull (= rtype :entity)
                  true)]
    (if visible
      ; Omit form-cell, we don't want any cell markup at all.
      [control field {} ctx])))
