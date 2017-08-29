(ns hypercrud.browser.base
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [cats.monad.exception :as exception]
            [hypercrud.browser.auto-anchor :as auto-anchor]
            [hypercrud.browser.user-bindings :as user-bindings]
            [hypercrud.client.core :as hc]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.util.core :as util]
            [hypercrud.util.monad :refer [exception->either]]
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

(defn meta-request-for-link [root-db link-dbid]             ; always latest
  (assert link-dbid)
  (->EntityRequest link-dbid nil root-db meta-pull-exp-for-link))

(defn request-for-link [link query-params param-ctx]
  (case (:request/type link)
    :query
    (mlet [q (-> (hc-string/safe-read-string (:link-query/value link)) exception->either)
           query-holes (-> ((exception/wrap q-util/parse-holes) q) exception->either)]
      (let [params-map (merge query-params (q-util/build-dbhole-lookup link param-ctx))
            params (->> query-holes (mapv (juxt identity #(get params-map %))) (into {}))
            pull-exp (->> (-> link :link-query/find-element (form-util/strip-forms-in-raw-mode param-ctx))
                          (mapv (juxt :find-element/name
                                      (fn [fe]
                                        (let [conn-id (q-util/fe-conn-id query-params fe)]
                                          [(hc/db (:peer param-ctx) conn-id (get-in param-ctx [:branches conn-id]))
                                           (q-util/form-pull-exp (:find-element/form fe))]))))
                          (into {}))
            ; todo validation of conns for pull-exp
            missing (->> params (filter (comp nil? second)) (mapv first))]
        (if (empty? missing)
          (cats/return (->QueryRequest q params pull-exp))
          (either/left {:message "missing param" :data {:params params :missing missing}}))))

    :entity
    (let [fe (first (filter #(= (:find-element/name %) "entity") (:link-query/find-element link)))
          conn-id (q-util/fe-conn-id query-params fe)]
      (cond
        (nil? conn-id) (either/left {:message "no connection" :data {:find-element fe}})
        (nil? (:entity query-params)) (either/left {:message "missing param" :data {:params query-params
                                                                                    :missing #{:entity}}})
        :else (either/right
                (->EntityRequest
                  (:entity query-params)
                  (:a query-params)
                  (hc/db (:peer param-ctx) conn-id (get-in param-ctx [:branches conn-id]))
                  (q-util/form-pull-exp (:find-element/form fe))))))

    :blank (either/right nil)

    (either/right nil)))

(defn process-results [get-f query-params link request result schemas param-ctx]
  (let [param-ctx (assoc param-ctx                          ; provide defaults before user-bindings run. TODO query side
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

        colspec (form-util/determine-colspec result link schemas query-params param-ctx)
        f (get-f link param-ctx)]
    (mlet [param-ctx (user-bindings/user-bindings' link param-ctx)]
      (cats/return
        (case (:display-mode param-ctx)                     ; default happens higher, it influences queries too
          :user (f result colspec (auto-anchor/auto-anchors link colspec query-params param-ctx) param-ctx)
          :xray (f result colspec (auto-anchor/auto-anchors link colspec query-params param-ctx) param-ctx)
          :root (f result colspec (auto-anchor/auto-anchors link colspec query-params param-ctx {:ignore-user-links true}) param-ctx))))))
