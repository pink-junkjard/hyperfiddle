(ns hypercrud.browser.links
  (:require [cljs.reader :as reader]
            [clojure.set :as set]
            [hypercrud.client.internal :as internal]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.util :as util]))


(defn link-type [link]
  (condp #(contains? %2 %1) (->> (keys (:link/request link)) (mapv namespace) set)
    "link-query" :link-query
    "link-entity" :link-entity))


; todo we shouldn't depend on an actual anchor/link, just that anchor/link's dbid
; the types for this function are too broad
(defn build-url-params-map [anchor param-ctx]
  #_(assert (not (empty? (-> anchor :anchor/link :link/request :link-query/find-element))) "dependent query insanity check")
  {:link-dbid (-> anchor :anchor/link :db/id)
   :query-params (->> (q-util/read-eval-formulas (:anchor/formula anchor))
                      (util/map-values #(q-util/run-formula % param-ctx)))}
  #_(condp = (link-type (:anchor/link anchor))
      :link-query {:link-dbid (-> anchor :anchor/link :db/id)
                   :query-params (->> (q-util/read-eval-formulas (:anchor/formula anchor))
                                      (util/map-values #(q-util/run-formula % param-ctx)))}
      :link-entity {:link-dbid (-> anchor :anchor/link :db/id)
                    :query-params (->> (q-util/read-eval-formulas (:anchor/formula anchor))
                                       (util/map-values #(q-util/run-formula % param-ctx)))
                    #_(let [find-element-name nil
                            attr (-> anchor :anchor/field :field/attribute :attribute/ident)]
                        (get-in param-ctx [:result find-element-name attr :db/id]))}))


(defn holes-filled? [hole-names params-map]
  (set/subset? (set hole-names) (set (keys (into {} (remove (comp nil? val) params-map))))))


(defn renderable-link? [link params-map]
  (if link
    (condp = (link-type link)
      :link-query (some-> link :link/request :link-query/value
                          reader/read-string
                          q-util/parse-holes
                          (holes-filled? params-map))
      :link-entity true)))


(defn build-link-props [anchor param-ctx]
  (let [param-ctx (assoc param-ctx :link-owner (-> anchor :anchor/link :hypercrud/owner)) ; tx-fn may need this
        tx-fn (if-let [tx-fn (:anchor/tx-fn anchor)]
                (let [{value :value error :error} (eval tx-fn)]
                  ;; non-fatal error, report it here so user can fix it
                  (if error (js/alert (str "cljs eval error: " error)))
                  value))]

    ;; add-result #(tx/edit-entity (:db/id entity) ident [] [(first %)])
    (if tx-fn
      {:on-click #((:user-swap! param-ctx) (tx-fn param-ctx))}
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
