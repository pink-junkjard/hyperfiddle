(ns hc.d
  (:require [cljs.pprint :as pprint]
            [hypercrud.react.react-fragment]
            [hypercrud.util.core :refer [pprint-str]]))


(defn a [window]
  (aset window "pr" cljs.core.pr)
  (aset window "pr_str" cljs.core.pr_str)
  (aset window "kw" keyword)
  (aset window "get" get)
  (aset window "pprint" pprint/pprint)
  (aset window "pprint_str" pprint-str)
  (aset window "hc_where" (fn [ctx]
                            (-> ctx
                                ; :relation, :db
                                (select-keys [:route :find-element :fe-pos :cell-data :attribute :value])
                                (update :attribute :db/ident)
                                (pprint-str 150))))
  (aset window "hc_route" (fn [ctx] (-> ctx :route pprint-str)))
  (aset window "hc_root_route" #(-> js/document.location.pathname
                                    hypercrud.browser.routing/decode
                                    pprint-str))
  (aset window "react_fragment" hypercrud.react.react-fragment/react-fragment)
  (aset window "dispatch" (fn [ctx action & args]
                            ; ((:dispatch! ctx) ())
                            ((:dispatch! ctx) (apply action args)))))


; cljs.core.get(ctx, kw('dispatch!'))(hyperfiddle.foundation.state.actions.toggle_staging())