(ns hyperfiddle.dev
  (:require [cljs.pprint :as pprint]                        ; clojure.pprint
            [hypercrud.browser.routing :as routing]
            [contrib.reagent :refer [fragment]]
            [contrib.data :refer [update-existing]]
            [contrib.string :refer [pprint-str]]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.foundation.actions :as foundation-actions]))


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
                                (select-keys [:cell-data
                                              :fe-pos
                                              :hypercrud.browser/attribute
                                              :hypercrud.browser/find-element
                                              :route
                                              :value])
                                (update-existing :hypercrud.browser/find-element deref)
                                (update-existing :cell-data deref)
                                (update-existing :value deref)
                                #_{:hypercrud.browser/attribute identity
                                   :fe-pos identity
                                   :hypercrud.browser/find-element deref
                                   :cell-data deref
                                   :route identity
                                   :value deref}
                                #_(reduce (fn [acc [k f]]
                                            (assoc acc k (f (get ctx k))))
                                          {})
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
          (runtime/dispatch! (:peer root-ctx) (foundation-actions/toggle-staging))))

  (aset global "transact"
        (fn []
          ; no idea what this means copy pasted from topnav
          (runtime/dispatch! (:peer root-ctx)
                             (foundation-actions/manual-transact! (:peer root-ctx)
                                                                  (:hypercrud.browser/invert-route root-ctx)
                                                                  {:hyperfiddle.ide/foo "page"}))))

  (aset global "root_ctx" root-ctx)

  (aset global "dispatch"
        (fn [ctx action & args]
          (runtime/dispatch! (:peer ctx) (apply action args))))
  )

; domain = cljs.core.get(main.main.root_ctx, kw('hypercrud.browser/domain'))
; thing = cljs.core.hash_map(kw('hyperfiddle.ide/foo'), "page")
; peer = cljs.core.get(main.main.root_ctx, kw('peer'))
; dispatch = cljs.core.get(main.main.root_ctx, kw('dispatch!'))
; dispatch(hyperfiddle.foundation.actions.manual_transact_BANG_(peer, domain, thing))