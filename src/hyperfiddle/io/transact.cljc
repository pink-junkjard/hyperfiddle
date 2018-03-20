(ns hyperfiddle.io.transact
  (:require #?(:clj [datomic.api :as d])
                    [hyperfiddle.io.http.core :refer [http-request!]]
                    [promesa.core :as p]))

#?(:clj
   (defn transact! [tx-groups]
     (let [valid? (every? (fn [[uri tx]]
                            (let [db (d/db (d/connect (str uri)))
                                  ; todo look up tx validator
                                  validate-tx (constantly true)]
                              (validate-tx db tx)))
                          tx-groups)]
       (if-not valid?
         (throw (RuntimeException. "user tx failed validation"))
         (let [tempid-lookups (->> tx-groups
                                   (mapv (fn [[uri dtx]]
                                           (let [{:keys [tempids]} @(d/transact (d/connect (str uri)) dtx)]
                                             [uri tempids])))
                                   (into {}))]
           {:tempid->id tempid-lookups})))))

(defn transact!-rpc! [service-uri tx-groups]
  (-> (http-request!
        {:url (str service-uri "transact")
         :accept :application/transit+json :as :auto
         :method :post :form tx-groups
         :content-type :application/transit+json})
      (p/then (fn [resp]
                (if (= 200 (:status resp))
                  ; clear master stage
                  ; but that has to be transactional with a redirect???
                  (p/resolved (:body resp))
                  (p/rejected resp))))))
