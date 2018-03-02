(ns hc.dev
  (:require [cljs.pprint :as pprint]
            [hypercrud.browser.routing :as routing]
            [hypercrud.react.react-fragment :as react-fragment]
            [hypercrud.util.core :refer [pprint-str]]
            [hyperfiddle.runtime :as runtime]))


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
                                (update :hypercrud.browser/find-element deref)
                                (update :cell-data deref)
                                (update :value deref)
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
  (aset global "react_fragment" react-fragment/react-fragment)
  (aset global "dispatch" (fn [ctx action & args]
                            (runtime/dispatch! (:peer ctx) (apply action args)))))


; cljs.core.get(ctx, kw('dispatch!'))(hyperfiddle.app.state.actions.toggle_staging())
