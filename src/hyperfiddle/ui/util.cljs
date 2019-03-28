(ns hyperfiddle.ui.util
  (:require
    [contrib.datomic-tx :as tx]
    [contrib.reactive :as r]
    [contrib.string :refer [empty->nil]]
    [hypercrud.browser.context :as context]
    [hyperfiddle.security.client :as security]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.runtime :as runtime]
    [taoensso.timbre :as timbre]))


(defn entity-change->tx                                     ; :Many editor is probably not idiomatic
  ([ctx vorvs]
    ; wut is going on with eav here in :many case
    ; the parent would still be in scope i guess
   (let [[_ a v] @(:hypercrud.browser/eav ctx)
         o (if (not= :db.type/ref (contrib.datomic/valueType @(:hypercrud.browser/schema ctx) a))
             v                                              ;(get entity a)      ; scalar
             (case (contrib.datomic/cardinality @(:hypercrud.browser/schema ctx) a) ; backwards refs good here? lol
               :db.cardinality/one v                        ;(context/smart-entity-identifier ctx (get entity a))
               :db.cardinality/many (map (partial context/smart-entity-identifier ctx) vorvs)))]
     (entity-change->tx ctx o vorvs)))
  ([ctx o n]
   (let [[e a v] @(:hypercrud.browser/eav ctx)
         attribute (context/hydrate-attribute! ctx a)
         n' (empty->nil n)]                                 ; hack for garbage string controls
     (when (and (some? n) (nil? n'))
       (timbre/warn "Trimming empty value to nil. This will be removed in a future release"))
     (tx/edit-entity e attribute o n'))))

(defn with-tx!
  ([ctx tx]
   (with-tx! ctx (context/dbname ctx) tx))
  ([ctx dbname tx]
   (->> (actions/with-groups (:peer ctx) (:branch ctx) {dbname tx})
        (runtime/dispatch! (:peer ctx)))))

(defn with-entity-change! [ctx] (r/comp (r/partial with-tx! ctx) (r/partial entity-change->tx ctx)))

(defn writable-entity? [ctx]
  (and
    ; If the db/id was not pulled, we cannot write through to the entity
    (boolean (let [[e a v] @(:hypercrud.browser/eav ctx)] e))
    @(r/track security/writable-entity? (:hypercrud.browser/parent ctx))))
