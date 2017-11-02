(ns hypercrud.browser.base
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either :refer-macros [try-either]]
            [hypercrud.browser.auto-anchor :as auto-anchor]
            [hypercrud.browser.auto-form :as auto-form]
            [hypercrud.browser.context-util :as context-util]
            [hypercrud.browser.q-util :as q-util]
            [hypercrud.browser.user-bindings :as user-bindings]
            [hypercrud.client.core :as hc]
            [hypercrud.types.Entity :refer [Entity]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.ThinEntity :refer [ThinEntity]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hypercrud.util.core :as util]
            [hypercrud.util.string :as hc-string]))


(def meta-pull-exp-for-link
  ['*
   :db/doc
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
  (let [link-id (get-in ctx [:route :link-id])
        _ (assert link-id "missing link-id")
        dbval (hc/db (:peer ctx) (:code-database-uri ctx) (:branch ctx))]
    (->EntityRequest link-id nil dbval meta-pull-exp-for-link)))

(letfn [(strip-form-in-raw-mode [fe ctx]
          (if (= @(:display-mode ctx) :root)
            (dissoc fe :find-element/form)
            fe))]
  (defn get-ordered-find-elements [link ctx]
    (mlet [fes (case (:request/type link)
                 :query (let [find-element-lookup (->> (:link-query/find-element link)
                                                       (map (juxt :find-element/name identity))
                                                       (into {}))]
                          (mlet [q (q-util/safe-parse-query-validated link)]
                            (->> (util/parse-query-element q :find)
                                 (mapv str)
                                 (mapv #(get find-element-lookup % {:find-element/name %}))
                                 (cats/return))))
                 :entity (either/right [(or (->> (:link-query/find-element link)
                                                 (filter #(= (:find-element/name %) "entity"))
                                                 first)
                                            {:find-element/name "entity"})])
                 (either/right []))]
      (->> fes
           (map #(into {} %))
           ; todo query-params should be inspected for Entity's and their conns
           (map (fn [fe] (update fe :find-element/connection #(or % "$"))))
           (map #(strip-form-in-raw-mode % ctx))
           (cats/return)))))

(defn request-for-link [link ordered-fes ctx]
  (case (:request/type link)
    :query
    (mlet [q (hc-string/memoized-safe-read-string (:link-query/value link))
           query-holes (try-either (q-util/parse-holes q))]
      (let [params-map (merge (:query-params ctx) (q-util/build-dbhole-lookup ctx))
            params (->> query-holes
                        (mapv (juxt identity (fn [hole-name]
                                               (let [param (get params-map hole-name)]
                                                 (cond
                                                   (instance? Entity param) (:db/id param)
                                                   (instance? ThinEntity param) (:db/id param)
                                                   :else param)))))
                        (into {}))
            pull-exp (->> ordered-fes
                          (mapv (juxt :find-element/name
                                      (fn [{:keys [:find-element/form :find-element/connection]}]
                                        (let [uri (context-util/ident->database-uri connection ctx)]
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
          ; todo if :entity query-param is a typed Entity, the connection is already provided. why are we ignoring?
          uri (context-util/ident->database-uri (:find-element/connection fe) ctx)
          e (get-in ctx [:query-params :entity])]
      (cond
        (nil? uri) (either/left {:message "no connection" :data {:find-element fe}})
        (nil? e) (either/left {:message "missing param" :data {:params (:query-params ctx)
                                                               :missing #{:entity}}})

        :else (either/right
                (->EntityRequest
                  (cond
                    (instance? Entity e) (:db/id e)
                    (instance? ThinEntity e) (:db/id e)
                    :else e)
                  (get-in ctx [:query-params :a])
                  (hc/db (:peer ctx) uri (:branch ctx))
                  (q-util/form-pull-exp (:find-element/form fe))))))

    :blank (either/right nil)

    (either/right nil)))

(let [never-read-only (constantly false)]
  (defn process-results [f link request result schemas ordered-fes ctx]
    (let [ctx (assoc ctx                        ; provide defaults before user-bindings run.
                      :schemas schemas                      ; For tx/entity->statements in userland.
                      :fiddle link                          ; for :db/doc
                      :read-only (or (:read-only ctx) never-read-only))

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
                     (instance? Entity result) {"entity" result}
                     (instance? ThinEntity result) {"entity" result}
                     (coll? result) (mapv (fn [relation] {"entity" relation}) result))

                   result)

          ordered-fes (auto-form/auto-find-elements ordered-fes result ctx)]
      (mlet [ctx (user-bindings/user-bindings' link ctx)]
        (cats/return
          (case @(:display-mode ctx)                  ; default happens higher, it influences queries too
            :user (f result ordered-fes (auto-anchor/auto-anchors link ordered-fes ctx) ctx)
            :xray (f result ordered-fes (auto-anchor/auto-anchors link ordered-fes ctx) ctx)
            :root (f result ordered-fes (auto-anchor/auto-anchors link ordered-fes ctx {:ignore-user-links true}) ctx)))))))
