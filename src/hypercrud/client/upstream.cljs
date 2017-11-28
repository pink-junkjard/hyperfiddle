(ns hypercrud.client.upstream
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [cuerdas.core :as str]
            [kvlt.core :as kvlt]
            [hypercrud.client.v-not-nil :refer [v-not-nil?]]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util]
            [hypercrud.client.process-result :as process-result]
            [promesa.core :as p]))


(defn sync [env uri-seq]
  (mlet [{:keys [body]} (kvlt/request! {:url (str (.-uri-str (:service-root-node env)) "sync")
                                        :accept :application/transit+json :as :auto
                                        :method :post :form (sort uri-seq)
                                        :content-type :application/transit+json})]
    (js/console.log "...global-basis!; response= " (str/prune (pr-str body) 100))
    (->> body
         (apply concat)
         (apply sorted-map))))

(defn- adapt-to-service-payload [requests stage-val]
  (let [staged-branches (->> stage-val
                             (mapcat (fn [[branch-ident branch-content]]
                                       (->> branch-content
                                            (map (fn [[uri tx]]
                                                   (let [branch-val (branch/branch-val uri branch-ident stage-val)]
                                                     {:branch-ident branch-ident
                                                      :branch-val branch-val
                                                      :uri uri
                                                      :tx (filter v-not-nil? tx)})))))))]
    {:staged-branches staged-branches :request requests}))

(defn hydrate-requests! [service-uri requests local-basis stage-val]       ; node only; browser hydrates by route
  ; Note the UI-facing interface is stage-val; hypercrud service accepts staged-branches
  (let [req (merge {:url (str (.-uri-str service-uri) "hydrate-requests/" ((comp base-64-url-safe/encode pr-str) local-basis)) ; serialize kvseq
                    :accept :application/transit+json :as :auto}
                   ; hydrate-requests always has a POST body, though it has a basis and is cachable
                   {:method :post
                    :form (adapt-to-service-payload requests stage-val)
                    :content-type :application/transit+json})]
    (js/console.log (str/format "...hydrate!; request count= %s basis= %s form= %s" (count requests) (pr-str local-basis) (str/prune (pr-str (:form req)) 100)))
    (-> (kvlt/request! req)
        (p/then (util/tee (fn [{:keys [body]}]
                            (js/console.log "...hydrate!; pulled-trees count= " (count (:pulled-trees body)))
                            (assert (= (count requests) (count (:pulled-trees body))) "Server contract violation; mismatched counts")
                            body)
                          #(js/console.log "...hydrate!; response= " (str/prune (pr-str %) 100)))))))

; Promise[List[Response]]
(defn hydrate-requests!* [service-uri requests local-basis stage-val]
  (-> (hydrate-requests! service-uri requests local-basis stage-val)
      (p/then (fn [{:keys [pulled-trees id->tempid]}]
                (either/branch
                  (->> pulled-trees (map #(process-result/process-result % requests)) cats/sequence)
                  p/rejected
                  p/resolved)))))

(defn hydrate-one! [service-uri request local-basis stage-val]
  #_ (defn hydrate-one! [service-uri request stage-val]
       (-> (http/hydrate! service-uri #{request} stage-val)
           (p/then (fn [{:keys [t pulled-trees-map id->tempid]}]
                     (if (contains? pulled-trees-map request)
                       (-> (get pulled-trees-map request)
                           (process-result request)
                           (either/branch p/rejected p/resolved))
                       (p/rejected {:message "Server failure"}))))))
  (hydrate-requests!* service-uri [request] local-basis stage-val))
