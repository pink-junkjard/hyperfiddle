(ns hypercrud.browser.base
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either :refer-macros [try-either]]
            [hypercrud.browser.auto-anchor :as auto-anchor]
            [hypercrud.browser.auto-form :as auto-form]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.user-bindings :as user-bindings]
            [hypercrud.client.core :as hc]
            [hypercrud.client.temp :as temp]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hypercrud.util.string :as hc-string]))


(def never-read-only (constantly false))

(def meta-pull-exp-for-link
  ['*
   :link-query/value
   :request/type
   ; get all our forms for this link
   {:link-query/find-element [:db/id
                              :find-element/name
                              :find-element/connection
                              {:find-element/form [:db/id
                                                   :form/name
                                                   {:form/field ['*]}]}]
    :link/anchor ['*
                  ; hydrate the whole link for validating the anchor by query params
                  {:anchor/link ['*]
                   :anchor/find-element [:db/id :find-element/name]}]}])

(defn meta-request-for-link [ctx]
  (let [link-id (get-in ctx [:route :link-dbid :id])
        _ (assert link-id "missing link-id")
        dbval (hc/db (:peer ctx) (:code-database-uri ctx) (:branch ctx))]
    (->EntityRequest link-id nil dbval meta-pull-exp-for-link)))

(defn request-for-link [link ordered-fes ctx]
  (case (:request/type link)
    :query
    (mlet [q (hc-string/memoized-safe-read-string (:link-query/value link))
           query-holes (try-either (q-util/parse-holes q))]
      (let [params-map (merge (:query-params ctx) (q-util/build-dbhole-lookup ctx))
            params (->> query-holes (mapv (juxt identity #(get params-map %))) (into {}))
            pull-exp (->> ordered-fes
                          (mapv (juxt :find-element/name
                                      (fn [{:keys [:find-element/form :find-element/connection]}]
                                        (let [uri (context/ident->database-uri connection ctx)]
                                          [(hc/db (:peer ctx) uri (:branch ctx))
                                           (q-util/form-pull-exp form)]))))
                          (into {}))
            ; todo validation of conns for pull-exp
            missing (->> params (filter (comp nil? second)) (mapv first))]
        (if (empty? missing)
          (cats/return (->QueryRequest q params pull-exp))
          (either/left {:message "missing param" :data {:params params :missing missing}}))))

    :entity
    (let [fe (first (filter #(= (:find-element/name %) "entity") ordered-fes))
          uri (context/ident->database-uri (:find-element/connection fe) ctx)
          e (get-in ctx [:query-params :entity :id])]
      (cond
        (nil? uri) (either/left {:message "no connection" :data {:find-element fe}})
        (nil? e) (either/left {:message "missing param" :data {:params (:query-params ctx)
                                                               :missing #{:entity}}})

        :else (either/right
                (->EntityRequest
                  e
                  (get-in ctx [:query-params :a])
                  (hc/db (:peer ctx) uri (:branch ctx))
                  (q-util/form-pull-exp (:find-element/form fe))))))

    :blank (either/right nil)

    (either/right nil)))

(defn process-results [f link request result schemas ordered-fes param-ctx]
  (let [param-ctx (assoc param-ctx                          ; provide defaults before user-bindings run.
                    :schemas schemas                        ; For tx/entity->statements in userland.
                    :read-only (or (:read-only param-ctx) never-read-only))

        ; ereq doesn't have a fe yet; wrap with a fe.
        ; Doesn't make sense to do on server since this is going to optimize away anyway.

        result (if (= :entity (:request/type link))
                 ; But the ereq might return a vec for cardinality many
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

                 result)

        ordered-fes (auto-form/auto-find-elements ordered-fes result param-ctx)]
    (mlet [param-ctx (user-bindings/user-bindings' link param-ctx)]
      (cats/return
        (case @(:display-mode param-ctx)                    ; default happens higher, it influences queries too
          :user (f result ordered-fes (auto-anchor/auto-anchors link ordered-fes param-ctx) param-ctx)
          :xray (f result ordered-fes (auto-anchor/auto-anchors link ordered-fes param-ctx) param-ctx)
          :root (f result ordered-fes (auto-anchor/auto-anchors link ordered-fes param-ctx {:ignore-user-links true}) param-ctx))))))
