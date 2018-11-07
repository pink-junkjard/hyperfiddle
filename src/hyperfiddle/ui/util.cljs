(ns hyperfiddle.ui.util
  (:require
    [cats.core :refer [>>=]]
    [cats.monad.either :as either]
    [clojure.set :refer [rename-keys]]
    [contrib.datomic-tx :as tx]
    [contrib.eval :as eval]
    [contrib.eval-cljs :as eval-cljs]
    [contrib.reactive :as r]
    [contrib.string :refer [blank->nil empty->nil]]
    [contrib.try$ :refer [try-either]]
    [hypercrud.browser.context :as context]
    [hyperfiddle.security.client :as security]
    [hyperfiddle.tempid :refer [smart-entity-identifier]]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.runtime :as runtime]))


(defn eval-cljs-ns [fiddle]
  (let [eval-in-ns 'user]                                   ; todo maybe use fiddle/ident for ns?
    (some->> @(r/cursor fiddle [:fiddle/cljs-ns]) blank->nil
             (eval-cljs/eval-statement-str! eval-in-ns)))
  nil)

; defer eval until render cycle inside userportal
(let [memoized-eval-string (memoize eval/eval-expr-str!+)]  ; don't actually need to safely eval, just want to memoize exceptions
  (defn eval-renderer-comp [fiddle-renderer-str & args]
    (either/branch
      (memoized-eval-string fiddle-renderer-str)
      (fn [e] (throw e))
      (fn [f] (into [f] args)))))

(defn entity-change->tx
  ([ctx n]
   (let [entity @(get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])
         attr-ident (last (:hypercrud.browser/path ctx))
         {:keys [:db/cardinality :db/valueType]} @(context/hydrate-attribute ctx attr-ident)
         o (if (not= (:db/ident valueType) :db.type/ref)
             (get entity attr-ident)
             (case (:db/ident cardinality)
               :db.cardinality/one (smart-entity-identifier ctx (get entity attr-ident))
               :db.cardinality/many (map (partial smart-entity-identifier ctx) (get entity attr-ident))))]
     (entity-change->tx ctx o n)))
  ([ctx o n]
   (let [id @(r/fmap (r/partial smart-entity-identifier ctx) (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))
         attribute @(context/hydrate-attribute ctx (last (:hypercrud.browser/path ctx)))
         n (empty->nil n)]                                  ; hack for garbage string controls
     (tx/edit-entity id attribute o n))))

(defn with-tx! [ctx tx]
  (let [uri (context/uri ctx)]
    (->> (actions/with-groups (:peer ctx) (:hypercrud.browser/invert-route ctx) (:branch ctx) {uri tx})
         (runtime/dispatch! (:peer ctx)))))

(defn with-entity-change! [ctx] (r/comp (r/partial with-tx! ctx) (r/partial entity-change->tx ctx)))

(defn writable-entity? [ctx]
  (and
    ; If the db/id was not pulled, we cannot write through to the entity
    (boolean @(r/fmap :db/id (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data] (r/track identity nil))))
    @(r/track security/writable-entity? (:hypercrud.browser/parent ctx))))
