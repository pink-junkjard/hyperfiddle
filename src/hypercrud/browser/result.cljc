(ns hypercrud.browser.result
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [datascript.parser :as parser]
            [hypercrud.util.non-fatal :refer [try-either]]
            [hypercrud.util.reactive :as reactive]))


(defn with-entity-relations
  "Process EntityRequest results into a relation or list of relations"
  [ctx & {:keys [:entity :attr-one :attr-many]}]
  (if-not @(reactive/fmap nil? (reactive/cursor (:hypercrud.browser/request ctx) [:a]))
    (let [[e a] (get-in ctx [:route :request-params])]
      (->> (try-either (.-dbname e))
           (cats/fmap
             (fn [source-symbol]
               (case @(reactive/cursor (:hypercrud.browser/schemas ctx) [(str source-symbol) a :db/cardinality :db/ident])
                 :db.cardinality/one
                 (attr-one (reactive/fmap vector (:hypercrud.browser/result ctx)))

                 :db.cardinality/many
                 (attr-many (reactive/fmap (reactive/partial mapv vector) (:hypercrud.browser/result ctx))))))))
    (either/right (entity (reactive/fmap vector (:hypercrud.browser/result ctx))))))

(defn with-query-relations
  "Process QueryRequest results into a relation or list of relations"
  [ctx & {:keys [:relation :relations]}]
  (->> (try-either (parser/parse-query @(reactive/cursor (:hypercrud.browser/request ctx) [:query])))
       (cats/fmap
         (fn [{:keys [qfind]}]
           (condp = (type qfind)
             datascript.parser.FindRel (relations (reactive/fmap (reactive/partial mapv vec) (:hypercrud.browser/result ctx)))
             datascript.parser.FindColl (relations (reactive/fmap (reactive/partial mapv vector) (:hypercrud.browser/result ctx)))
             datascript.parser.FindTuple (relation (reactive/fmap vec (:hypercrud.browser/result ctx)))
             datascript.parser.FindScalar (relation (reactive/fmap vector (:hypercrud.browser/result ctx))))))))
