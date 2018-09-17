(ns hyperfiddle.ui.util
  (:require
    [cats.core :refer [>>=]]
    [cats.monad.either :as either]
    [clojure.set :refer [rename-keys]]
    [contrib.datomic-tx :as tx]
    [contrib.eval :as eval]
    [contrib.reactive :as r]
    [contrib.string :refer [empty->nil]]
    [contrib.try$ :refer [try-either]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security :as security]
    [hyperfiddle.tempid :refer [smart-entity-identifier]]))


; defer eval until render cycle inside userportal
(let [safe-eval-string #(try-either (when % (eval/eval-string! %))) ; don't actually need to safely eval, just want to memoize exceptions
      memoized-eval-string (memoize safe-eval-string)]
  (defn eval-renderer-comp [?fiddle-cljs-ns-str fiddle-renderer-str & args]
    (let [+result (>>= (memoized-eval-string ?fiddle-cljs-ns-str)
                      (r/constantly
                        ; eval ns for the effect on the cljs namespaces
                        (memoized-eval-string fiddle-renderer-str)))]
      (either/branch
        +result
        (fn [e]
          (throw e))
        (fn [f] (into [f] args))))))

(defn readonly->disabled [props]                            ; this utility is harmful and should be removed
  ; :placeholder, etc is allowed and pass through
  (rename-keys props {:read-only :disabled}))

(defn on-change->tx [ctx o n]
  (let [id @(r/fmap (r/partial smart-entity-identifier ctx) (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))
        attribute @(context/hydrate-attribute ctx (last (:hypercrud.browser/path ctx)))]
    (tx/edit-entity id attribute o (empty->nil n))))

(defn entity-change->tx [ctx new-val]                       ; legacy todo remove
  (let [entity @(get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])
        attr-ident (last (:hypercrud.browser/path ctx))
        {:keys [:db/cardinality :db/valueType]} @(context/hydrate-attribute ctx attr-ident)
        o (if (not= (:db/ident valueType) :db.type/ref)
            (get entity attr-ident)
            (case (:db/ident cardinality)
              :db.cardinality/one (smart-entity-identifier ctx (get entity attr-ident))
              :db.cardinality/many (map (partial smart-entity-identifier ctx) (get entity attr-ident))))]
    (on-change->tx ctx o new-val)))

(let [parent-m (fn parent-m [ctx]
                 (when-let [ctx (:hypercrud.browser/parent ctx)]
                   (let [ident @(r/cursor (:hypercrud.browser/field ctx) [::field/path-segment])
                         isComponent @(context/hydrate-attribute ctx ident :db/isComponent)]
                     (if isComponent
                       (parent-m ctx)
                       @(:hypercrud.browser/data ctx)))))]
  (defn writable-entity? [ctx]
    (and
      ; If the db/id was not pulled, we cannot write through to the entity
      (boolean @(r/fmap :db/id (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))
      (if-let [m (parent-m ctx)]
        (let [dbname (context/dbname ctx)
              hf-db (domain/dbname->hfdb dbname (:hypercrud.browser/domain ctx))
              subject @(runtime/state (:peer ctx) [::runtime/user-id])]
          (security/writable-entity? hf-db subject m))
        false                                               ; todo should client sec have control of this case?
        ))))

(let [on-change (fn [ctx o n]
                  (->> (on-change->tx ctx o n)
                       (context/with-tx! ctx)))]
  (defn entity-props
    ([props ctx]
     (-> (assoc props :on-change (r/partial on-change ctx))
         (update :read-only #(or % (not @(r/track writable-entity? ctx))))))
    ; val is complected convenience arity, coincidentally most consumers need to assoc :value
    ([val props ctx] (entity-props (assoc props :value val) ctx))))
