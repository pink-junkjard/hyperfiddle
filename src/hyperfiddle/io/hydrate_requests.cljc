(ns hyperfiddle.io.hydrate-requests
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [cuerdas.core :as str]
            [hypercrud.browser.router :as router]
    #?(:clj
            [hyperfiddle.io.datomic.hydrate-requests :as datomic-hydrate-requests])
            [hyperfiddle.io.http.core :refer [http-request!]]
            [hyperfiddle.io.util :refer [process-result v-not-nil?]]
            [hyperfiddle.runtime :as runtime]
            [promesa.core :as p]
            [taoensso.timbre :as timbre]))


(defn stage-val->staged-branches [stage-val]
  (->> stage-val
       (mapcat (fn [[branch-ident branch-content]]
                 (->> branch-content
                      (map (fn [[uri tx]]
                             {:branch-ident branch-ident
                              :uri uri
                              :tx (filter v-not-nil? tx)})))))))

(defn hydrate-one! [rt local-basis stage request]
  (-> (runtime/hydrate-requests rt local-basis stage [request])
      (p/then (fn [{:keys [pulled-trees]}]
                (-> (process-result (first pulled-trees) request)
                    (either/branch p/rejected p/resolved))))))

; Promise[List[Response]]
(defn hydrate-all-or-nothing! [rt local-basis stage requests]
  (-> (runtime/hydrate-requests rt local-basis stage requests)
      (p/then (fn [{:keys [pulled-trees]}]
                (-> (map process-result pulled-trees requests)
                    (cats/sequence)
                    (either/branch p/rejected p/resolved))))))

(defn hydrate-requests-rpc! [service-uri local-basis stage-val requests & [jwt]]
  (let [staged-branches (stage-val->staged-branches stage-val)
        ; Note the UI-facing interface contains stage-val; the API accepts staged-branches
        req (into {:url (str service-uri "hydrate-requests/" (router/-encode-pchar local-basis)) ; serialize kvseq
                   :accept :application/transit+json :as :auto
                   :method :post                            ; hydrate-requests always has a POST body, though it has a basis and is cachable
                   :form {:staged-branches staged-branches :request requests}
                   :content-type :application/transit+json}
                  (when jwt {:auth {:bearer jwt}}))]
    (timbre/debugf "hydrate-requests! request count= %s basis= %s form= %s" (count requests) (pr-str local-basis) (str/prune (pr-str (:form req)) 100))
    (-> (http-request! req)
        (p/then (fn [{:keys [body]}]
                  (assert (= (count requests) (count (:pulled-trees body))) "Server contract violation; mismatched counts")
                  body)))))

#?(:clj
   (def hydrate-requests datomic-hydrate-requests/hydrate-requests))
