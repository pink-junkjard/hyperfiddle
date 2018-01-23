(ns hc.dev
  (:require [cljs.pprint :as pprint]
            [hypercrud.browser.routing :as routing]
            [hypercrud.react.react-fragment :as react-fragment]
            [hypercrud.util.core :refer [pprint-str]]))


(defn set-globals [global]
  (aset global "pr" cljs.core.pr)
  (aset global "pr_str" cljs.core.pr_str)
  (aset global "kw" keyword)
  (aset global "get" get)
  (aset global "pprint" pprint/pprint)
  (aset global "pprint_str" pprint-str)
  (aset global "hc_where" (fn [ctx]
                            (-> ctx
                                ; :relation, :db
                                (select-keys [:route :find-element :fe-pos :cell-data :attribute :value])
                                (update :attribute :db/ident)
                                (update :cell-data deref)
                                (update :value deref)
                                (pprint-str 150))))
  (aset global "hc_route" (fn [ctx] (-> ctx :route pprint-str)))
  (aset global "hc_root_route" #(-> js/document.location.pathname
                                    routing/decode
                                    pprint-str))
  (aset global "react_fragment" react-fragment/react-fragment)
  (aset global "dispatch" (fn [ctx action & args]
                            ; ((:dispatch! ctx) ())
                            ((:dispatch! ctx) (apply action args)))))


; cljs.core.get(ctx, kw('dispatch!'))(hyperfiddle.app.state.actions.toggle_staging())
