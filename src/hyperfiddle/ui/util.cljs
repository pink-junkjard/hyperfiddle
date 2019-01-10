(ns hyperfiddle.ui.util
  (:require
    [contrib.datomic-tx :as tx]
    [contrib.reactive :as r]
    [contrib.string :refer [empty->nil]]
    [hypercrud.browser.context :as context]
    [hyperfiddle.security.client :as security]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.runtime :as runtime]))


(defn entity-change->tx
  ([ctx n]
   (let [entity @(get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])
         attr-ident (last (:hypercrud.browser/path ctx))
         {:keys [:db/cardinality :db/valueType]} @(context/hydrate-attribute ctx attr-ident)
         o (if (not= (:db/ident valueType) :db.type/ref)
             (get entity attr-ident)
             (case (:db/ident cardinality)
               :db.cardinality/one (context/smart-entity-identifier ctx (get entity attr-ident))
               :db.cardinality/many (map (partial context/smart-entity-identifier ctx) (get entity attr-ident))))]
     (entity-change->tx ctx o n)))
  ([ctx o n]
   (let [id @(r/fmap (r/partial context/smart-entity-identifier ctx) (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))
         attribute @(context/hydrate-attribute ctx (last (:hypercrud.browser/path ctx)))
         n (empty->nil n)]                                  ; hack for garbage string controls
     (tx/edit-entity id attribute o n))))

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
    (boolean @(r/fmap :db/id (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data] (r/track identity nil))))
    @(r/track security/writable-entity? (:hypercrud.browser/parent ctx))))
