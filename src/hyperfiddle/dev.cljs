(ns hyperfiddle.dev
  (:require
    [contrib.data :refer [update-existing]]
    [contrib.pprint :refer [pprint pprint-str]]
    [contrib.reagent :refer [fragment]]
    [goog.object :as object]
    [hypercrud.browser.routing :as routing]
    [hyperfiddle.runtime :as runtime]))


(def ^:dynamic root-ctx)                                    ; debug backdoor to dispatch!

(defn set-globals [global]
  (object/set global "pr" cljs.core.pr)
  (object/set global "pr_str" cljs.core.pr_str)
  (object/set global "kw" keyword)
  (object/set global "get" get)
  (object/set global "pprint" pprint)
  (object/set global "pprint_str" pprint-str)
  (object/set global "hc_where" (fn [ctx]
                                  (-> ctx
                                      (select-keys [:hypercrud.browser/route  ; ordered for glance debugging
                                                    :hypercrud.browser/data
                                                    :hypercrud.browser/field
                                                    :hypercrud.browser/path])
                                      (update-existing :hypercrud.browser/route deref)
                                      (update-existing :hypercrud.browser/data deref)
                                      (update-existing :hypercrud.browser/field deref)
                                      (update-existing :hypercrud.browser/parent select-keys [:hypercrud.browser/field :hypercrud.browser/path])
                                      (pprint-str 150))))
  (object/set global "hc_route" (fn [ctx] (-> ctx :hypercrud.browser/route deref pprint-str)))
  (object/set global "hc_root_route" (fn []
                                       (js/console.warn "WARNING: hc_root_route needs to use the runtime for decoding, this will be broken with custom user routing")
                                       (-> js/document.location.pathname
                                           routing/decode
                                           pprint-str)))
  (object/set global "react_fragment" fragment)

  (object/set global "root_ctx" root-ctx)

  (object/set global "dispatch"
              (fn [ctx action & args]
                (runtime/dispatch! (:peer ctx) (apply action args))))
  )

; domain = cljs.core.get(main.main.root_ctx, kw('hypercrud.browser/domain'))
; thing = cljs.core.hash_map(kw('hyperfiddle.ide/foo'), "page")
; peer = cljs.core.get(main.main.root_ctx, kw('peer'))
; dispatch = cljs.core.get(main.main.root_ctx, kw('dispatch!'))
; dispatch(hyperfiddle.actions.manual_transact_BANG_(peer, domain, thing))