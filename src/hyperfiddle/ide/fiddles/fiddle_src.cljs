(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require [contrib.data :refer [kwargs unwrap]]
            [contrib.reagent :refer [fragment]]
            [hypercrud.ui.control.markdown-rendered :refer [markdown]]
            [hypercrud.ui.tooltip :refer [tooltip]]
            [hyperfiddle.ide.fiddles.topnav :refer [shadow-fiddle hijack-renderer loading-spinner]]))


(defn fiddle-src-renderer [ctx class]
  #_(hypercrud.ui.result/fiddle ctx)
  (let [ctx (-> (shadow-fiddle ctx) (dissoc :user-renderer))
        rtype (:fiddle/type @(:hypercrud.browser/result ctx))]
    [:div {:class class}
     [:h3 "fiddle src"]
     ((:cell ctx) [true 0 :fiddle/ident] ctx)
     ((:cell ctx) [true 0 :fiddle/type] ctx)
     (case rtype
       :entity ((:cell ctx) [true 0 :fiddle/pull] ctx)
       :query ((:cell ctx) [true 0 :fiddle/query] ctx)
       :blank nil
       nil nil)
     ((:cell ctx) [true 0 :fiddle/markdown] ctx)
     ((:cell ctx) [true 0 :fiddle/renderer] ctx)
     ((:cell ctx) [true 0 :fiddle/css] ctx)
     ((:cell ctx) [true 0 :fiddle/entrypoint?] ctx)
     ((:cell ctx) [true 0 :fiddle/bindings] ctx)
     ((:anchor ctx) :hyperfiddle/remove [0] ctx "Remove fiddle")
     ((:browse ctx) :attribute-renderers [] ctx (partial hijack-renderer true))
     ((:browse ctx) :links [] ctx (partial hijack-renderer true))
     ]))
