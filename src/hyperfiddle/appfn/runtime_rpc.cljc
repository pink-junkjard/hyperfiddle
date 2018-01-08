(ns hyperfiddle.appfn.runtime-rpc                                  ;-impl
  (:require [cuerdas.core :as str]
            [hypercrud.http.core :refer [http-request!]]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]
            [hyperfiddle.appfn.runtime-local :refer [stage-val->staged-branches]]
            [promesa.core :as p]
            [taoensso.timbre :as timbre]))


(defn hydrate-requests! [service-uri local-basis stage-val requests]
  (let [staged-branches (stage-val->staged-branches stage-val)
        ; Note the UI-facing interface contains stage-val; the API accepts staged-branches
        req {:url (str #?(:clj service-uri :cljs (.-uri-str service-uri)) "hydrate-requests/" ((comp base-64-url-safe/encode pr-str) local-basis)) ; serialize kvseq
             :accept :application/transit+json :as :auto
             :method :post                                  ; hydrate-requests always has a POST body, though it has a basis and is cachable
             :form {:staged-branches staged-branches :request requests}
             :content-type :application/transit+json}]
    (timbre/debugf "hydrate-requests! request count= %s basis= %s form= %s" (count requests) (pr-str local-basis) (str/prune (pr-str (:form req)) 100))
    (-> (http-request! req)
        (p/then (fn [{:keys [body]}]
                  (assert (= (count requests) (count (:pulled-trees body))) "Server contract violation; mismatched counts")
                  body)))))

(defn transact!! [service-uri tx-groups]
  (-> (http-request!
        {:url (str #?(:clj service-uri :cljs (.-uri-str service-uri)) "transact")
         :accept :application/transit+json :as :auto
         :method :post :form tx-groups
         :content-type :application/transit+json})
      (p/then (fn [resp]
                (if (= 200 (:status resp))
                  ; clear master stage
                  ; but that has to be transactional with a redirect???
                  (p/resolved (:body resp))
                  (p/rejected resp))))))

(defn sync! [service-uri dbs]
  (-> (http-request! {:url (str #?(:clj service-uri :cljs (.-uri-str service-uri)) "sync")
                 :accept :application/transit+json :as :auto
                 :method :post :form dbs
                 :content-type :application/transit+json})
      (p/then (fn [{:keys [body]}]
                (->> body
                     (apply concat)
                     (apply sorted-map))))))
