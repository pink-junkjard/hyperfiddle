(ns hypercrud.client.http
  (:require [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [hypercrud.client.transit :as transit]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util]
            [kvlt.core :as kvlt]
            [kvlt.middleware.params]
            [promesa.core :as p]
            [clojure.string :as string]))


(def content-type-transit "application/transit+json; charset=utf-8")
(def content-type-edn "application/edn; charset=utf-8")


(defmethod kvlt.middleware.params/coerce-form-params (keyword content-type-transit) [{:keys [form-params]}]
  (transit/encode form-params))


(defmethod kvlt.middleware/from-content-type (keyword content-type-transit) [resp]
  (let [decoded-val (transit/decode (:body resp))]
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

(defn hydrate! [service-uri requests stage-val]             ; node only; browser hydrates route
  (let [staged-branches (->> stage-val
                             (mapcat (fn [[branch-ident branch-content]]
                                       (->> branch-content
                                            (map (fn [[uri tx]]
                                                   (let [branch-val (branch/branch-val uri branch-ident stage-val)]
                                                     {:branch-ident branch-ident
                                                      :branch-val branch-val
                                                      :uri uri
                                                      :tx (filter v-not-nil? tx)})))))))]
    (-> (kvlt/request! {:url (str (.-uri-str service-uri) "hydrate")
                        :accept content-type-transit :as :auto
                        :content-type content-type-transit
                        :method :post :form {:staged-branches staged-branches :request requests}})
        (p/then #(-> % :body :hypercrud)))))

(defn hydrate-route! [service-uri route stage-val]
  (let [staged-branches (->> stage-val
                             (mapcat (fn [[branch-ident branch-content]]
                                       (->> branch-content
                                            (map (fn [[uri tx]]
                                                   (let [branch-val (branch/branch-val uri branch-ident stage-val)]
                                                     {:branch-ident branch-ident
                                                      :branch-val branch-val
                                                      :uri uri
                                                      :tx (filter v-not-nil? tx)})))))))
        req (merge {:url (str (.-uri-str service-uri) "hydrate-route" route)
                    :accept content-type-transit :as :auto}
                   (if (empty? stage-val)
                     {:method :get}                         ; Try to hit CDN
                     {:method :post
                      :form {:staged-branches staged-branches}
                      :content-type content-type-transit}))]
    (-> (kvlt/request! req) (p/then :body))))

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
