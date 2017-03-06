(ns hypercrud.browser.links
  (:require [cljs.reader :as reader]
            [clojure.set :as set]
            [hypercrud.client.internal :as internal]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.util :as util]
            [promesa.core :as p]))


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


(defn holes-filled? [hole-names params-map]
  (set/subset? (set hole-names) (set (keys (into {} (remove (comp nil? val) params-map))))))


(defn renderable-link? [link params-map]
  (case (link-type link)
    :link-query (some-> link :link/request :link-query/value
                        reader/read-string
                        q-util/parse-holes
                        (holes-filled? params-map))
    :link-entity true
    false))


(defn build-link-props [anchor param-ctx]
  (let [param-ctx (assoc param-ctx :link-owner (-> anchor :anchor/link :hypercrud/owner)) ; tx-fn may need this
        tx-fn (if-let [tx-fn (:anchor/tx-fn anchor)]
                (let [{value :value error :error} (eval tx-fn)]
                  ;; non-fatal error, report it here so user can fix it
                  (if error (js/alert (str "cljs eval error: " error)))
                  value))]

    ;; add-result #(tx/edit-entity (:db/id entity) ident [] [(first %)])
    (if tx-fn
      {:on-click #(let [result (tx-fn param-ctx)]
                    ; tx-fn can be sync or async, based on return instance.
                    (-> (if-not (p/promise? result) (p/resolved result) result)
                        (p/then (:user-swap! param-ctx))))}
      (let [params-map (build-url-params-map anchor param-ctx)]
        {:route params-map
         :class (if-not (renderable-link? (:anchor/link anchor) params-map)
                  "invalid")}))))


(defn link-visible? [anchor param-ctx]
  (let [visible-src (:anchor/visible? anchor)
        visible-fn (if-not (empty? visible-src)
                     (let [{:keys [value error]} (eval visible-src)]
                       (if error (js/alert (str "cljs eval error: " error)))
                       value)
                     (constantly true))]
    (visible-fn param-ctx)))
