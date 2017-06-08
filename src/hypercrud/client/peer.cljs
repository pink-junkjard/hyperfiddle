(ns hypercrud.client.peer
  (:require [clojure.set :as set]
            [hypercrud.client.core :as hc]
            [hypercrud.client.http :as http]
            [hypercrud.client.response :as response]
            [hypercrud.util.core :as util]
            [promesa.core :as p]))


(defn- hydrate! [entry-uri requests stage-val]
  (-> (http/hydrate! entry-uri requests stage-val)
      (p/then (fn [{:keys [t pulled-trees-map]}]
                (response/->Response (into #{} requests) pulled-trees-map stage-val)))))

(deftype Peer [entry-uri stage ^:mutable last-response]
  hc/Peer
  ; why did 'force?' behavior change?
  (hydrate! [this request]
    #_(if (hc/hydrated? this request)                       ; this if check should be higher?
        (p/resolved last-response))
    (-> (hydrate! entry-uri request @stage)
        (p/then (fn [response]
                  (set! last-response response)
                  last-response))))

  ; for clone link - is this bad? yeah its bad since it can never be batched.
  (hydrate-one! [this request]
    (-> (hydrate! entry-uri #{request} @stage)
        (p/then (fn [response] (hc/hydrate response request)))))

  (hydrated? [this requests]
    ; compare our pre-loaded state with the peer dependencies
    (set/subset? (set requests) (some-> last-response .-requests)))

  (transact! [this]
    (let [htx-groups (->> @stage
                          (util/map-values (fn [branch-tx]
                                             (->> (get branch-tx nil)
                                                  (filter (fn [[op e a v]]
                                                            (not (and (or (= :db/add op) (= :db/retract op))
                                                                      (nil? v)))))))))]
      (http/transact! entry-uri htx-groups))))
