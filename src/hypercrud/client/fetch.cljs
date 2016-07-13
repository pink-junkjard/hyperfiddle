(ns hypercrud.client.fetch
  (:require [goog.Uri]
            [hypercrud.client.util :as util]
            [kvlt.middleware.params]
            [kvlt.core :as kvlt]
            [promesa.core :as p]))


(defn resolve-root-relative-uri [^goog.Uri entry-uri ^goog.Uri relative-uri]
  (-> (.clone entry-uri)
      (.resolve relative-uri)))


(def content-type-transit "application/transit+json;charset=UTF-8")


(defmethod kvlt.middleware.params/coerce-form-params (keyword content-type-transit) [{:keys [form-params]}]
  (util/transit-encode form-params))


(defmethod kvlt.middleware/from-content-type (keyword content-type-transit) [resp]
  (let [decoded-val (util/transit-decode (:body resp))]
    (assoc resp :body decoded-val)))


(defn fetch! [user-token entry-uri relative-uri]
  (let [start (.now js/Date)]
    (-> (kvlt/request!
          {:url (-> (resolve-root-relative-uri entry-uri relative-uri)
                    (.setParameterValue "user-token" user-token))
           :accept content-type-transit
           :method :get
           :as :auto})
        (p/finally #(do (println (str "Request took: " (- (.now js/Date) start) "ms")) %)))))


(defn query! [user-token entry-uri relative-uri query params]
  (kvlt/request!
    {:url (-> (resolve-root-relative-uri entry-uri relative-uri)
              (.setParameterValue "user-token" user-token))
     :content-type content-type-transit
     :accept content-type-transit
     :method :post
     :form [query params]
     :as :auto}))


(defn transact! [user-token entry-uri txs]
  (kvlt/request!
    {:url (-> (resolve-root-relative-uri entry-uri (goog.Uri. "/api/transact"))
              (.setParameterValue "user-token" user-token))
     :content-type content-type-transit
     :accept content-type-transit
     :method :post
     :form txs
     :as :auto}))
