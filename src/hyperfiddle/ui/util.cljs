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
    [hypercrud.browser.context :as context]))


; defer eval until render cycle inside userportal
(let [safe-eval-string #(try-either (when % (eval/eval-string %))) ; don't actually need to safely eval, just want to memoize exceptions
      memoized-eval-string (memoize safe-eval-string)]
  (defn eval-renderer-comp [?fiddle-cljs-ns-str fiddle-renderer-str & args]
    (let [result (>>= (memoized-eval-string ?fiddle-cljs-ns-str)
                      (r/constantly
                        ; eval ns for the effect on the cljs namespaces
                        (memoized-eval-string fiddle-renderer-str)))]
      (either/branch
        result
        (fn [e]
          (throw e))
        (fn [f] (into [f] args))))))

(defn readonly->disabled [props]                            ; this utility is harmful and should be removed
  ; :placeholder, etc is allowed and pass through
  (rename-keys props {:read-only :disabled}))

(defn on-change->tx [ctx o n]
  (let [id @(r/fmap :db/id (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))
        attribute @(context/hydrate-attribute ctx (last (:hypercrud.browser/path ctx)))]
    (tx/edit-entity id attribute o (empty->nil n))))

(defn entity-change->tx [ctx new-val]                       ; legacy todo remove
  (let [entity @(get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])
        attr-ident (last (:hypercrud.browser/path ctx))
        {:keys [:db/cardinality :db/valueType]} @(context/hydrate-attribute ctx attr-ident)
        o (if (not= (:db/ident valueType) :db.type/ref)
            (get entity attr-ident)
            (case (:db/ident cardinality)
              :db.cardinality/one (get-in entity [attr-ident :db/id])
              :db.cardinality/many (->> (get entity attr-ident)
                                        (map :db/id))))]
    (on-change->tx ctx o new-val)))

(defn writable-entity? [entity-val]
  ; If the db/id was not pulled, we cannot write through to the entity
  (cljs.core/boolean (:db/id entity-val)))

(let [on-change (fn [ctx o n]
                  (->> (on-change->tx ctx o n)
                       (context/with-tx! ctx)))]
  (defn entity-props
    ([props ctx]
     (-> (assoc props :on-change (r/partial on-change ctx))
         (update :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))))
    ; val is complected convenience arity, coincidentally most consumers need to assoc :value
    ([val props ctx] (entity-props (assoc props :value val) ctx))))
