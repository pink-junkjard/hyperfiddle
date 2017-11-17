(ns hypercrud.client.peer
  (:require [cats.monad.either :as either]
            [cuerdas.core :as str]
            [hypercrud.client.core :as hc]
            [hypercrud.client.http :as http]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.types.Err :refer [Err]]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util]
            [promesa.core :as p]
            [reagent.core :as reagent]))


(defn human-error [e req]
  (let [unfilled-holes (->> (filter (comp nil? val) (.-params req)) (map key))]
    (if-not (empty? unfilled-holes)
      {:message "Invalid query" :data {:datomic-error (.-msg e) :query (.-query req) :missing unfilled-holes}}
      {:message "Datomic error" :data {:datomic-error (.-msg e)}})))

(defn process-result [resultset-or-error request]
  #_(js/console.log "...process-result; r=" (pr-str resultset-or-error))
  (if (instance? Err resultset-or-error)
    (either/left (human-error resultset-or-error request))
    (either/right resultset-or-error)))

(defn hydrate-one! [service-uri request stage-val]
  (js/console.log "...hydrate-one!; request count= " (count request))
  (-> (http/hydrate! service-uri [request] stage-val)
      (p/then (fn [{:keys [pulled-trees id->tempid] :as result}]
                (js/console.log "...hydrate-one!; pulled-trees count= " (count pulled-trees))
                (js/console.log "...hydrate-one!; result= " (util/pprint-str result 100))
                (if-not (= 1 (count pulled-trees))
                  (p/rejected (str/format "Server contract violation; count request= 1 count result= %s" (count pulled-trees)))
                  (-> (some-> (nth pulled-trees 0) (process-result request))
                      (either/branch p/rejected p/resolved)))))))

(deftype Peer [state-atom]
  hc/Peer
  (hydrate [this request]
    (if-let [result @(reagent/cursor state-atom [:ptm request])]
      (process-result result request)
      (either/left {:message "Loading" :data {:request request}})))

  (db [this uri branch]
    (->DbVal uri (branch/branch-val uri branch @(reagent/cursor state-atom [:stage]))))

  IHash
  (-hash [this] (goog/getUid this)))
