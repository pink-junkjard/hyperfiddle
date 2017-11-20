(ns hypercrud.client.upstream
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [cuerdas.core :as str]
            [kvlt.core :as kvlt]
            [hypercrud.client.origin :refer [v-not-nil?]]
            [hypercrud.types.Err :refer [Err]]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util]
            [promesa.core :as p]))


(defn sync [env uri-seq]
  (mlet [{:keys [body]} (kvlt/request! {:url (str (.-uri-str (:service-root-node env)) "sync")
                                        :accept :application/transit+json :as :auto
                                        :method :post :form uri-seq :content-type :application/transit+json})]
    (js/console.log "...global-basis!; response= " (str/prune (pr-str body) 100))
    body))

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

(defn hydrate! [service-uri requests basis stage-val]       ; node only; browser hydrates by route
  ; Note the UI-facing interface is stage-val; hypercrud service accepts staged-branches
  (js/console.log "...hydrate!; top; stage-val= " (pr-str stage-val))
  (let [req (merge {:url (str (.-uri-str service-uri) "hydrate/" basis)
                    :accept :application/transit+json :as :auto}
                   (if (empty? stage-val)
                     {:method :get}
                     {:method :post
                      :form (adapt-to-service-payload requests stage-val)
                      :content-type :application/transit+json}))]
    (js/console.log (str/format "...hydrate!; request count= %s basis= %s form= %s" (count requests) basis (str/prune (pr-str (:form req)) 100)))
    (-> (kvlt/request! req)
        (p/then (util/tee (fn [{:keys [body]}]
                            (js/console.log "...hydrate!; pulled-trees count= " (count (:pulled-trees body)))
                            (assert (= (count requests) (count (:pulled-trees body))) "Server contract violation; mismatched counts")
                            body)
                          #(js/console.log "...hydrate!; response= " (str/prune (pr-str %) 100)))))))


(defn human-error [e req]
  (let [unfilled-holes (->> (filter (comp nil? val) (.-params req)) (map key))]
    (if-not (empty? unfilled-holes)
      {:message "Invalid query" :data {:datomic-error (.-msg e) :query (.-query req) :missing unfilled-holes}}
      {:message "Datomic error" :data {:datomic-error (.-msg e)}})))

(defn process-result [resultset-or-error request]
  (if (instance? Err resultset-or-error)
    (either/left (human-error resultset-or-error request))
    (either/right resultset-or-error)))

(defn hydrate-all! [service-uri requests basis stage-val]
  (mlet [{:keys [pulled-trees id->tempid]} (hydrate! service-uri requests basis stage-val)]
    (let [result (->> pulled-trees (map #(process-result % requests)) sequence)]
      (either/branch result p/rejected p/resolved))))

(defn hydrate-one! [service-uri request basis stage-val]
  (hydrate-all! service-uri [request] stage-val basis))
