(ns hypercrud.browser.base
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either :refer-macros [try-either]]
            [hypercrud.browser.auto-anchor :as auto-anchor]
            [hypercrud.browser.auto-form :as auto-form]
            [hypercrud.browser.user-bindings :as user-bindings]
            [hypercrud.client.core :as hc]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hypercrud.util.string :as hc-string]))


(def never-read-only (constantly false))

(def meta-pull-exp-for-link
  (let [form-pull-exp ['*
                       {:hypercrud/owner ['*]
                        :form/field ['*]}]]
    ['*
     :link-query/value
     :link-query/single-result-as-entity?
     :request/type
     {:link-query/dbhole ['* {:dbhole/value ['*]}]
      ; get all our forms for this link
      :link-query/find-element ['* {:find-element/form form-pull-exp
                                    :find-element/connection [:db/id :database/ident]}]
      :link/anchor ['*
                    {:anchor/link ['*                       ; hydrate the whole link for validating the anchor by query params
                                   {:hypercrud/owner ['*]}] ; need the link's owner to render the href to it
                     :anchor/find-element [:db/id :find-element/name :find-element/connection]}]
      :hypercrud/owner ['*]}]))

(defn meta-request-for-link [link-dbid param-ctx]
  (assert link-dbid)
  (let [dbval (hc/db (:peer param-ctx) hc/*root-conn-id* (:branch param-ctx))]
    (->EntityRequest link-dbid nil dbval meta-pull-exp-for-link)))

(defn request-for-link [link query-params ordered-fes param-ctx]
  (case (:request/type link)
    :query
    (mlet [q (hc-string/memoized-safe-read-string (:link-query/value link))
           query-holes (try-either (q-util/parse-holes q))]
      (let [params-map (merge (q-util/build-dbhole-lookup link param-ctx) query-params)
            params (->> query-holes (mapv (juxt identity #(get params-map %))) (into {}))
            pull-exp (->> ordered-fes
                          (mapv (juxt :find-element/name
                                      (fn [fe]
                                        (let [conn-id (-> fe :find-element/connection :db/id :id)]
                                          [(hc/db (:peer param-ctx) conn-id (:branch param-ctx))
                                           (q-util/form-pull-exp (:find-element/form fe))]))))
                          (into {}))
            ; todo validation of conns for pull-exp
            missing (->> params (filter (comp nil? second)) (mapv first))]
        (if (empty? missing)
          (cats/return (->QueryRequest q params pull-exp))
          (either/left {:message "missing param" :data {:params params :missing missing}}))))

    :entity
    (let [fe (first (filter #(= (:find-element/name %) "entity") ordered-fes))
          conn-id (-> fe :find-element/connection :db/id :id)]
      (cond
        (nil? conn-id) (either/left {:message "no connection" :data {:find-element fe}})
        (nil? (:entity query-params)) (either/left {:message "missing param" :data {:params query-params
                                                                                    :missing #{:entity}}})
        :else (either/right
                (->EntityRequest
                  (:entity query-params)
                  (:a query-params)
                  (hc/db (:peer param-ctx) conn-id (:branch param-ctx))
                  (q-util/form-pull-exp (:find-element/form fe))))))

    :blank (either/right nil)

    (either/right nil)))

(defn process-results [f query-params link request result schemas ordered-fes param-ctx]
  (let [param-ctx (assoc param-ctx                          ; provide defaults before user-bindings run.
                    :schemas schemas                        ; For tx/entity->statements in userland.
                    :query-params query-params
                    :read-only (or (:read-only param-ctx) never-read-only))

        ; ereq doesn't have a fe yet; wrap with a fe.
        ; Doesn't make sense to do on server since this is going to optimize away anyway.

        result (case (:request/type link)
                 :entity                                    ; But the ereq might return a vec for cardinality many
                 (cond
                   ; order matters here a lot!
                   (nil? result) nil
                   (empty? result) (if (.-a request)
                                     ; comes back as [] sometimes if cardinaltiy many request. this is causing problems as nil or {} in different places.
                                     ; Above comment seems backwards, left it as is
                                     (case (let [fe-name (->> (:link-query/find-element link)
                                                              (filter #(= (:find-element/name %) "entity"))
                                                              first
                                                              :find-element/name)]
                                             (get-in schemas [fe-name (.-a request) :db/cardinality :db/ident]))
                                       :db.cardinality/one {}
                                       :db.cardinality/many []))
                   (map? result) {"entity" result}
                   (coll? result) (mapv (fn [relation] {"entity" relation}) result))

                 :query
                 (cond
                   (-> link :link-query/single-result-as-entity?) (first result)
                   :else result)

                 result)

        ordered-fes (auto-form/auto-find-elements ordered-fes result param-ctx)]
    (mlet [param-ctx (user-bindings/user-bindings' link param-ctx)]
      (cats/return
        (case @(:display-mode param-ctx)                    ; default happens higher, it influences queries too
          :user (f result ordered-fes (auto-anchor/auto-anchors link ordered-fes param-ctx) param-ctx)
          :xray (f result ordered-fes (auto-anchor/auto-anchors link ordered-fes param-ctx) param-ctx)
          :root (f result ordered-fes (auto-anchor/auto-anchors link ordered-fes param-ctx {:ignore-user-links true}) param-ctx))))))
