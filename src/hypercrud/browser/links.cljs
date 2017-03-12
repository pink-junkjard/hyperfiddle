(ns hypercrud.browser.links
  (:require [cljs.reader :as reader]
            [clojure.set :as set]
            [hypercrud.client.internal :as internal]
            [hypercrud.compile.eval :refer [eval-str]]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.util :as util]
            [promesa.core :as p]
            [hypercrud.browser.connection-color :as connection-color]))


(defn link-type [link]
  (condp #(contains? %2 %1) (->> (keys (:link/request link)) (mapv namespace) set)
    "link-query" :link-query
    "link-entity" :link-entity
    nil))


(defn build-url-params-map
  ([domain project link-dbid formula param-ctx]
   {:domain domain
    :project project
    :link-dbid link-dbid #_:id
    :query-params (->> (q-util/read-eval-formulas formula)
                       (util/map-values #(q-util/run-formula % param-ctx)))})
  ([link formula param-ctx]
   (build-url-params-map
     (-> link :hypercrud/owner :database/domain)
     (-> link :hypercrud/owner :database/ident)
     (-> link :db/id)
     formula
     param-ctx))
  ([anchor param-ctx]
   (build-url-params-map (:anchor/link anchor) (:anchor/formula anchor) param-ctx))
  #_(case (link-type (:anchor/link anchor))
      :link-query {:link-dbid (-> anchor :anchor/link :db/id)
                   :query-params (->> (q-util/read-eval-formulas (:anchor/formula anchor))
                                      (util/map-values #(q-util/run-formula % param-ctx)))}
      :link-entity {:link-dbid (-> anchor :anchor/link :db/id)
                    :query-params (->> (q-util/read-eval-formulas (:anchor/formula anchor))
                                       (util/map-values #(q-util/run-formula % param-ctx)))
                    #_(let [find-element-name nil
                            attr (-> anchor :anchor/attribute :attribute/ident)]
                        (get-in param-ctx [:result find-element-name attr :db/id]))}))


(defn holes-filled? [hole-names query-params-map]
  (set/subset? (set hole-names) (set (keys (into {} (remove (comp nil? val) query-params-map))))))

(defn anchor-valid? [link url-params]                       ; could return monad to say why
  ; We specifically hydrate this deep just so we can validate anchors like this.
  (case (link-type link)
    :link-query (-> link :link/request :link-query/value
                    reader/read-string q-util/parse-param-holes
                    (holes-filled? (:query-params url-params)))
    :link-entity (contains? (:query-params url-params) :entity-dbid-s)
    false))

(defn anchor-tooltip [link url-params param-ctx]
  (case (:display-mode param-ctx)
    :undressed (if (anchor-valid? link url-params)
                 [nil (pr-str (:query-params url-params))]
                 [:warning (pr-str (:query-params url-params))])
    nil))

(defn build-link-props [anchor param-ctx]
  (let [param-ctx (assoc param-ctx :link-owner (-> anchor :anchor/link :hypercrud/owner)) ; tx-fn may need this
        tx-fn (if-let [tx-fn (:anchor/tx-fn anchor)]
                (let [{value :value error :error} (eval-str tx-fn)]
                  ;; non-fatal error, report it here so user can fix it
                  (if error (js/alert (str "cljs eval error: " error))) ; return monad so tooltip can draw the error
                  value))]
    (if tx-fn
      {:on-click #(let [result (tx-fn param-ctx)]           ; tx-fn may be sync or async
                    (-> (if-not (p/promise? result) (p/resolved result) result)
                        (p/then (:user-swap! param-ctx))))}
      (let [url-params (build-url-params-map anchor param-ctx) #_"return monad so tooltip can draw the error?"]
        {:route url-params
         :style {:color (connection-color/connection-color (-> anchor :anchor/link :hypercrud/owner :db/id :id))}
         :tooltip (anchor-tooltip (:anchor/link anchor) url-params param-ctx)
         :class (if-not (anchor-valid? (:anchor/link anchor) url-params) "invalid")}))))

(defn link-visible? [anchor param-ctx]
  (let [visible-src (:anchor/visible? anchor)
        visible-fn (if-not (empty? visible-src)
                     (let [{:keys [value error]} (eval-str visible-src)]
                       (if error (js/alert (str "cljs eval error: " error)))
                       value)
                     (constantly true))]
    (visible-fn param-ctx)))
