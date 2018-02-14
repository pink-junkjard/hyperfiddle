(ns hypercrud.browser.browser-request
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [datascript.parser :as parser]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.link :as link]
            [hypercrud.browser.popovers :as popovers]
            [hypercrud.browser.result :as result]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.util.core :refer [unwrap]]
            [hypercrud.util.non-fatal :refer [try-catch-non-fatal try-either]]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.runtime :as runtime]
            [taoensso.timbre :as timbre]))


(declare request-from-route)
(declare request-from-link)

(defn recurse-request [link ctx]
  (if (:link/managed? link)
    (let [route' (routing/build-route' link ctx)
          popover-id (popovers/popover-id link ctx)
          ctx (context/anchor-branch ctx link)]
      (if @(runtime/state  (:peer ctx) [:popovers popover-id])
        ; if the anchor IS a popover, we need to run the same logic as anchor/build-anchor-props
        ; the ctx needs to be updated (branched, etc), but NOT BEFORE determining the route
        ; that MUST happen in the parent context
        (let [ctx (-> ctx
                      (context/clean)
                      (update :hypercrud.browser/debug #(str % ">popover-link[" (:db/id link) ":" (or (:link/rel link) (:anchor/prompt link)) "]")))]
          (either/branch route'
                         (constantly nil)
                         #(request-from-route % ctx)))))
    ; if the anchor IS NOT a popover, this should be the same logic as widget/render-inline-anchors
    (let [ctx (update ctx :hypercrud.browser/debug #(str % ">inline-link[" (:db/id link) ":" (or (:link/rel link) (:anchor/prompt link)) "]"))]
      (request-from-link link ctx))))

(defn cell-dependent-requests [ctx]
  (let [ctx (context/cell-data ctx)]
    (concat
      (->> (link/links-lookup (:links ctx) [(:fe-pos ctx)])
           (filter :link/dependent?)
           (mapcat #(recurse-request % ctx)))
      (->> (get-in ctx [:find-element :fields])
           (mapcat (fn [field]
                     (let [ctx (-> (context/attribute ctx (:attribute field))
                                   (context/value (reactive/map (:cell-data->value field) (:cell-data ctx))))]
                       (->> (link/links-lookup (:links ctx) [(:fe-pos ctx) (-> ctx :attribute :db/ident)])
                            (filter :link/dependent?)
                            (mapcat #(recurse-request % ctx))))))))))

(defn relation-dependent-requests [ctx]
  (->> (result/map-relation cell-dependent-requests ctx)
       (apply concat)))

(defn fiddle-dependent-requests [result ordered-fes links ctx]
  ; reconcile this with the link.cljc logic
  (let [links (filter :link/render-inline? links)           ; at this point we only care about inline links
        ctx (assoc ctx
              :ordered-fes ordered-fes
              :links links)]
    (concat
      (->> (mapcat #(recurse-request % ctx) (->> (link/links-lookup (:links ctx) [])
                                                 (remove :link/dependent?))))
      (->> (:ordered-fes ctx)                               ; might have empty results
           (map-indexed (fn [fe-pos fe]
                          (let [ctx (context/find-element ctx fe fe-pos)]
                            (concat
                              (->> (link/links-lookup (:links ctx) [fe-pos])
                                   (remove :link/dependent?)
                                   (mapcat #(recurse-request % ctx)))
                              (->> (:fields fe)
                                   (mapcat (fn [{:keys [attribute]}]
                                             (let [ctx (context/attribute ctx attribute)]
                                               (->> (link/links-lookup (:links ctx) [fe-pos (-> ctx :attribute :db/ident)])
                                                    (remove :link/dependent?)
                                                    (mapcat #(recurse-request % ctx)))))))))))
           (apply concat))
      (case (get-in ctx [:fiddle :fiddle/type])
        :entity (if-let [a (get-in ctx [:request :a])]
                  (let [[e a] (get-in ctx [:route :request-params])]
                    (either/branch
                      (try-either (.-dbname e))
                      (constantly nil)
                      (fn [source-symbol]
                        (case (get-in ctx [:schemas (str source-symbol) a :db/cardinality :db/ident])
                          :db.cardinality/one
                          (let [ctx (context/relation ctx (reactive/atom [result]))]
                            (relation-dependent-requests ctx))

                          :db.cardinality/many
                          (let [ctx (context/relations ctx (mapv vector result))]
                            (->> (result/map-relations relation-dependent-requests ctx)
                                 (apply concat)))))))
                  (let [ctx (context/relation ctx (reactive/atom [result]))]
                    (relation-dependent-requests ctx)))

        :query (either/branch
                 (try-either (parser/parse-query (get-in ctx [:request :query])))
                 (constantly nil)
                 (fn [{:keys [qfind]}]
                   (condp = (type qfind)
                     datascript.parser.FindRel
                     (let [ctx (context/relations ctx (mapv vec result))]
                       (->> (result/map-relations relation-dependent-requests ctx)
                            (apply concat)))

                     datascript.parser.FindColl
                     (let [ctx (context/relations ctx (mapv vector result))]
                       (->> (result/map-relations relation-dependent-requests ctx)
                            (apply concat)))

                     datascript.parser.FindTuple
                     (let [ctx (context/relation ctx (reactive/atom (vec result)))]
                       (relation-dependent-requests ctx))

                     datascript.parser.FindScalar
                     (let [ctx (context/relation ctx (reactive/atom [result]))]
                       (relation-dependent-requests ctx)))))

        :blank nil

        nil))))

(defn f-mode-config []
  {:from-ctx :user-request
   :from-link :fiddle/request
   :with-user-fn (fn [user-fn]
                   (fn [result ordered-fes links ctx]
                     ; todo report invocation errors back to the user
                     ; user-fn HAS to return a seqable value, we want to throw right here if it doesn't
                     (try-catch-non-fatal (seq (user-fn result ordered-fes links ctx))
                                          e (do
                                              (timbre/error e)
                                              nil))))
   :default fiddle-dependent-requests})

(defn process-data [{:keys [result ordered-fes links ctx]}]
  (mlet [request-fn (base/fn-from-mode (f-mode-config) (:fiddle ctx) ctx)]
    (cats/return (request-fn @result ordered-fes links ctx))))

(defn request-from-route [route ctx]
  (let [ctx (context/route ctx route)
        {:keys [meta-fiddle-req' fiddle']} (base/hydrate-fiddle ctx)]
    (concat (if-let [meta-fiddle-req (unwrap meta-fiddle-req')]
              [meta-fiddle-req])
            (unwrap
              (mlet [fiddle fiddle'
                     fiddle-request (base/request-for-fiddle fiddle ctx)]
                (cats/return
                  (concat
                    (if fiddle-request [fiddle-request])
                    (schema-util/schema-requests-for-link ctx)
                    (-> (base/process-results fiddle fiddle-request ctx)
                        (cats/bind process-data)
                        unwrap))))))))

(defn request-from-link [link ctx]
  (unwrap (base/from-link link ctx (fn [route ctx]
                                     (either/right (request-from-route route ctx))))))
