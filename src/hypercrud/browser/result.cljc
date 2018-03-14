(ns hypercrud.browser.result
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [datascript.parser :as parser]
            [hypercrud.browser.context :as context]
            [hypercrud.util.non-fatal :refer [try-either]]
            [hypercrud.util.reactive :as reactive]))


(defn with-entity-relations
  "Process EntityRequest results into a relation or list of relations"
  [ctx]
  (if-not @(reactive/fmap nil? (reactive/cursor (:hypercrud.browser/request ctx) [:a]))
    (let [[e a] (get-in ctx [:route :request-params])]
      (->> (try-either (.-dbname e))
           (cats/fmap
             (fn [source-symbol]
               (case @(reactive/cursor (:hypercrud.browser/schemas ctx) [(str source-symbol) a :db/cardinality :db/ident])
                 :db.cardinality/one
                 (context/relation ctx (reactive/fmap vector (:hypercrud.browser/result ctx)))

                 :db.cardinality/many
                 (context/relations ctx (reactive/fmap (reactive/partial mapv vector) (:hypercrud.browser/result ctx))))))))
    (either/right (context/relation ctx (reactive/fmap vector (:hypercrud.browser/result ctx))))))

(defn with-query-relations
  "Process QueryRequest results into a relation or list of relations"
  [ctx]
  (->> (try-either (parser/parse-query @(reactive/cursor (:hypercrud.browser/request ctx) [:query])))
       (cats/fmap
         (fn [{:keys [qfind]}]
           (condp = (type qfind)
             datascript.parser.FindRel (context/relations ctx (reactive/fmap (reactive/partial mapv vec) (:hypercrud.browser/result ctx)))
             datascript.parser.FindColl (context/relations ctx (reactive/fmap (reactive/partial mapv vector) (:hypercrud.browser/result ctx)))
             datascript.parser.FindTuple (context/relation ctx (reactive/fmap vec (:hypercrud.browser/result ctx)))
             datascript.parser.FindScalar (context/relation ctx (reactive/fmap vector (:hypercrud.browser/result ctx))))))))

(defn with-relations [ctx]
  (case @(reactive/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/type]) ; fiddle/type not relevant outside this fn
    :entity (with-entity-relations ctx)
    :query (with-query-relations ctx)
    :blank (either/right ctx)
    (either/right ctx)))
