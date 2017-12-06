(ns hypercrud.api.util
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [hypercrud.api.core :as api]
            [hypercrud.types.Err :refer [Err]]
            [promesa.core :as p]))


(defn human-error [e req]
  (let [unfilled-holes (->> (filter (comp nil? val) (.-params req)) (map key))]
    ; what about EntityRequests? why are datomic errors not sufficient?
    (if-not (empty? unfilled-holes)
      {:message "Invalid query" :data {:datomic-error (.-msg e) :query (.-query req) :missing unfilled-holes}}
      {:message "Datomic error" :data {:datomic-error (.-msg e)}})))

; this can be removed; #err can natively be Either
(defn process-result [resultset-or-error request]
  (if (instance? Err resultset-or-error)
    (either/left (human-error resultset-or-error request))
    (either/right resultset-or-error)))

(defn v-not-nil? [stmt]
  (or (map? stmt)
      (let [[op e a v] stmt]
        (not (and (or (= :db/add op) (= :db/retract op))
                  (nil? v))))))

(defn hydrate-one! [rt local-basis stage request]
  (-> (api/hydrate-requests rt local-basis stage #{request})
      (p/then (fn [{:keys [pulled-trees-map]}]
                (if (contains? pulled-trees-map request)
                  (-> (get pulled-trees-map request)
                      (process-result request)
                      (either/branch p/rejected p/resolved))
                  (p/rejected {:message "Server failure"}))))))

; Promise[List[Response]]
(defn hydrate-all-or-nothing! [rt local-basis stage requests]
  (-> (api/hydrate-requests rt local-basis stage requests)
      (p/then (fn [{:keys [pulled-trees]}]
                (either/branch
                  (->> pulled-trees
                       (map #(process-result % requests)) cats/sequence)
                  p/rejected
                  p/resolved)))))
