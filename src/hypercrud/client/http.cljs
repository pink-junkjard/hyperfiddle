(ns hypercrud.client.http
  (:require [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [hypercrud.client.internal :as internal]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util]
            [kvlt.core :as kvlt]
            [kvlt.middleware.params]
            [promesa.core :as p]))


(def content-type-transit "application/transit+json;charset=UTF-8")
(def content-type-edn "application/edn;charset=UTF-8")


(defmethod kvlt.middleware.params/coerce-form-params (keyword content-type-transit) [{:keys [form-params]}]
  (internal/transit-encode form-params))


(defmethod kvlt.middleware/from-content-type (keyword content-type-transit) [resp]
  (let [decoded-val (internal/transit-decode (:body resp))]
    (assoc resp :body decoded-val)))


(defmethod kvlt.middleware.params/coerce-form-params (keyword content-type-edn) [{:keys [form-params]}]
  (binding [pprint/*print-miser-width* nil
            pprint/*print-right-margin* 200]
    (with-out-str (pprint/pprint form-params))))


(defmethod kvlt.middleware/from-content-type (keyword content-type-edn) [resp]
  (let [decoded-val (reader/read-string (:body resp))]
    (assoc resp :body decoded-val)))

(defn v-not-nil? [stmt]
  (or (map? stmt)
      (let [[op e a v] stmt]
        (not (and (or (= :db/add op) (= :db/retract op))
                  (nil? v))))))

(defn hydrate! [service-uri requests stage-val]
  (let [branch-vals (->> stage-val
                         (mapcat (fn [[branch branch-content]]
                                   (->> branch-content
                                        (map (fn [[uri tx]]
                                               (let [tx (branch/db-content uri branch stage-val)]
                                                 [[uri (hash tx)] (filter v-not-nil? tx)]))))))
                         (into {}))]
    (-> (kvlt/request! {:url (str (.-uri-str service-uri) "hydrate")
                        :content-type content-type-transit  ; helps debugging to view as edn
                        :accept content-type-transit        ; needs to be fast so transit
                        :method :post
                        :form {:staged-tx branch-vals :request requests}
                        :as :auto})
        (p/then #(-> % :body :hypercrud)))))

(defn transact! [service-uri htx-groups]
  (let [htx-groups (->> (get htx-groups nil)
                        (util/map-values (partial filter v-not-nil?)))]
    (-> (kvlt/request!
          {:url (str (.-uri-str service-uri) "transact")
           :content-type content-type-transit
           :accept content-type-transit
           :method :post
           :form htx-groups
           :as :auto})
        (p/then (fn [resp]
                  (if (= 200 (:status resp))
                    ; clear master stage
                    ; but that has to be transactional with a redirect???
                    (p/resolved (-> resp :body :hypercrud))
                    (p/rejected resp)))))))
