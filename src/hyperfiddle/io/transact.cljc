(ns hyperfiddle.io.transact
  (:require [cats.monad.either :as either]
            [contrib.eval :as eval]
    #?(:clj
            [datomic.api :as d])
            [hyperfiddle.io.http.core :refer [http-request!]]
            [hyperfiddle.security :as security]
            [promesa.core :as p]
            [taoensso.timbre :as timbre]))

#?(:clj
   (let [memoized-safe-eval-string (memoize eval/safe-eval-string+)]
     (defn process-tx [domains-uri subject uri tx]
       (let [hf-db (-> (d/db (d/connect (str domains-uri)))
                       (d/entity [:database/uri uri]))
             f (case (:database/write-security hf-db ::security/allow-anonymous) ; todo yank this default
                 ::security/owner-only security/write-owner-only
                 ::security/authenticated-users-only security/write-authenticated-users-only
                 ::security/allow-anonymous security/write-allow-anonymous
                 ::security/custom (-> (memoized-safe-eval-string (:database.custom-security/server hf-db))
                                       (either/branch
                                         (fn [e]
                                           (timbre/error e)
                                           (throw (ex-info "Misconfigured database security" {:hyperfiddle.io/http-status-code 500
                                                                                              :uri uri
                                                                                              :additional-info (.getMessage e)})))
                                         :process-tx)))]
         (f hf-db subject tx)))))

#?(:clj
   (defn transact! [domains-uri subject tx-groups]
     (let [tempid-lookups (->> tx-groups
                               (map (fn [[uri tx]]
                                      [uri (process-tx domains-uri subject uri tx)]))
                               (doall)                      ; allow any exceptions to fire before transacting anythng
                               (map (fn [[uri dtx]]
                                      (let [{:keys [tempids]} @(d/transact (d/connect (str uri)) dtx)]
                                        [uri tempids])))
                               (into {}))]
       {:tempid->id tempid-lookups})))

(defn transact!-rpc! [service-uri tx-groups & [jwt]]
  (-> {:url (str service-uri "transact")
       :accept :application/transit+json :as :auto
       :method :post :form tx-groups
       :content-type :application/transit+json}
      (into (when jwt {:auth {:bearer jwt}}))
      (http-request!)
      (p/then (fn [resp]
                (if (= 200 (:status resp))
                  ; clear master stage
                  ; but that has to be transactional with a redirect???
                  (p/resolved (:body resp))
                  (p/rejected resp))))))
