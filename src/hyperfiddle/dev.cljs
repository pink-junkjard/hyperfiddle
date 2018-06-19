(ns hyperfiddle.dev
  (:require [cljs.pprint :as pprint]                        ; clojure.pprint
            [contrib.data :refer [update-existing]]
            [contrib.reagent :refer [fragment]]
            [contrib.pprint :refer [pprint-str]]
            [hypercrud.browser.routing :as routing]
            [hyperfiddle.actions :as actions]
            [hyperfiddle.runtime :as runtime]
            [hypercrud.browser.context :as context]))


(def ^:dynamic root-ctx)                                    ; debug backdoor to dispatch!

; cljs.core.get(ctx, kw('dispatch!'))(hyperfiddle.app.state.actions.toggle_staging())

(defn set-globals [global]
  (aset global "pr" cljs.core.pr)
  (aset global "pr_str" cljs.core.pr_str)
  (aset global "kw" keyword)
  (aset global "get" get)
  (aset global "pprint" pprint/pprint)
  (aset global "pprint_str" pprint-str)
  (aset global "hc_where" (fn [ctx]
                            (-> ctx
                                (select-keys [:route        ; ordered for glance debugging
                                              :hypercrud.browser/attribute
                                              :fe-pos
                                              :hypercrud.browser/find-element
                                              :relation
                                              :relations])
                                (update-existing :hypercrud.browser/find-element deref)
                                (update-existing :relations deref)
                                (update-existing :relation deref)
                                (update-existing :cell-data deref)
                                (assoc :cell-data @(context/entity ctx))
                                (assoc :value @(context/value ctx))
                                (pprint-str 150))))
  (aset global "hc_route" (fn [ctx] (-> ctx :route pprint-str)))
  (aset global "hc_root_route" (fn []
                                 (js/console.warn "WARNING: hc_root_route needs to use the runtime for decoding, this will be broken with custom user routing")
                                 (-> js/document.location.pathname
                                     routing/decode
                                     pprint-str)))
  (aset global "react_fragment" fragment)

  (aset global "toggle_stage"
        (fn []
          (runtime/dispatch! (:peer root-ctx) (actions/toggle-staging))))

  (aset global "root_ctx" root-ctx)

  (aset global "dispatch"
        (fn [ctx action & args]
          (runtime/dispatch! (:peer ctx) (apply action args))))
  )

; domain = cljs.core.get(main.main.root_ctx, kw('hypercrud.browser/domain'))
; thing = cljs.core.hash_map(kw('hyperfiddle.ide/foo'), "page")
; peer = cljs.core.get(main.main.root_ctx, kw('peer'))
; dispatch = cljs.core.get(main.main.root_ctx, kw('dispatch!'))
; dispatch(hyperfiddle.actions.manual_transact_BANG_(peer, domain, thing))