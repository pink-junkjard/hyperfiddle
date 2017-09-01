(ns hypercrud.client.http
  (:require [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [goog.Uri]
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


(defn resolve-relative-uri [^goog.Uri entry-uri ^goog.Uri relative-uri]
  (-> (.clone entry-uri)
      (.resolve relative-uri)))

(defn purge-nil-vals [hctx-groups]
  (let [v-not-nil? (fn [[op e a v]]
                     (not (and (or (= :db/add op) (= :db/retract op))
                               (nil? v))))]
    (->> hctx-groups
         (util/map-values #(util/map-values (partial filter v-not-nil?) %)))))

(defn hydrate! [entry-uri requests stage-val]
  (let [branch-vals (->> stage-val
                         (mapcat (fn [[branch branch-content]]
                                   (->> branch-content
                                        (map (fn [[conn-id tx]]
                                               (let [tx (branch/db-content conn-id branch stage-val)]
                                                 [[conn-id (hash tx)] tx]))))))
                         #_purge-nil-vals
                         (into {}))]
    (-> (kvlt/request! {:url (resolve-relative-uri entry-uri (goog.Uri. "hydrate"))
                        :content-type content-type-transit  ; helps debugging to view as edn
                        :accept content-type-transit        ; needs to be fast so transit
                        :method :post
                        :form {:staged-tx branch-vals :request requests}
                        :as :auto})
        (p/then #(-> % :body :hypercrud)))))

(defn transact! [entry-uri htx-groups]
  (let [htx-groups (-> (purge-nil-vals htx-groups)
                       (get nil))]
    (-> (kvlt/request!
          {:url (resolve-relative-uri entry-uri (goog.Uri. "transact"))
           :content-type content-type-edn
           :accept content-type-edn
           :method :post
           :form htx-groups
           :as :auto})
        (p/then (fn [resp]
                  (if (= 200 (:status resp))
                    ; clear master stage
                    ; but that has to be transactional with a redirect???
                    (p/resolved (-> resp :body :hypercrud))
                    (p/rejected resp)))))))
