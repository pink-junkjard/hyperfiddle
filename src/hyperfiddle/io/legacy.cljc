(ns hyperfiddle.io.legacy
  (:require
    [cats.monad.either :as either]
    [contrib.datomic-errors :refer [datomic-error-cleaner]]
    [hypercrud.types.Err :refer [#?(:cljs Err)]])
  #?(:clj
     (:import (hypercrud.types.Err Err))))


(defn human-error [e req]
  ; this is invalid on the jvm
  #_(let [unfilled-holes (->> (filter (comp nil? val) (.-params req)) (map key))]
      ; what about EntityRequests? why are datomic errors not sufficient?
      (if-not (empty? unfilled-holes)
        {:message "Invalid query" :data {:datomic-error (.-msg e) :query (.-query req) :missing unfilled-holes}}))
  (datomic-error-cleaner (.-msg e) req))

; this can be removed; #err can natively be Either
(defn process-result [resultset-or-error request]
  (if (instance? Err resultset-or-error)
    (either/left (human-error resultset-or-error request))
    (either/right resultset-or-error)))

(defn stage->staged-branches [stage-val]
  (->> stage-val
       (mapcat (fn [[branch-ident branch-content]]
                 (->> branch-content
                      (map (fn [[dbname tx]]
                             {:branch-ident branch-ident
                              :dbname dbname
                              :tx tx})))))))
