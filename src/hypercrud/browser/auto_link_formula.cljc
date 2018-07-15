(ns hypercrud.browser.auto-link-formula                     ; namespace is public export runtime
  (:require [cats.monad.either :as either]
            [contrib.data :refer [abs-normalized]]
            [contrib.reactive :as r]
            [contrib.string :refer [memoized-safe-read-edn-string]]
            [hypercrud.browser.dbname :as dbname]
            [hypercrud.browser.field :as field]
            [hypercrud.browser.context :as context]
            [hypercrud.types.ThinEntity :refer [->ThinEntity]]
            [hypercrud.util.branch :as branch]
            [hyperfiddle.runtime :as runtime]
            [taoensso.timbre :as timbre]))


(defn ^:export auto-entity-from-stage [ctx]
  ; This returns a new value each time the transaction changes - can't call it again later.
  ; So tx-fns must inspect the modal-route, they can't re-create the dbid.
  (assert (:uri ctx) "no uri in dynamic scope (If it can't be inferred, add as bindings)")
  (let [branch-val (branch/branch-val (:uri ctx) (:branch ctx) @(runtime/state (:peer ctx) [:stage]))
        id (-> branch-val hash abs-normalized - str)]
    (->ThinEntity (dbname/uri->dbname (:uri ctx) ctx) id)))

; todo there are collisions when two links share the same 'location'
(letfn [(hash-data [ctx]
          (when-let [data (:hypercrud.browser/data ctx)]
            (case (:hypercrud.browser/data-cardinality ctx)
              :db.cardinality/one @(r/fmap :db/id data)
              :db.cardinality/many (hash (into #{} @(r/fmap (partial mapv :db/id) data))) ; todo scalar
              nil nil #_":db/id has a faked attribute with no cardinality, need more thought to make elegant")))]
  (defn deterministic-ident [ctx]
    (-> (str (:hypercrud.browser/path ctx) "."
             ; todo recurse all the way up the path?
             ; in essence just data + parent-data is relative not fully qualified, which technically is not unique
             (hash-data (:hypercrud.browser/parent ctx)) "."
             (hash-data ctx))
        hash abs-normalized - str)))

(defn ^:export auto-entity [ctx]
  (->ThinEntity (dbname/uri->dbname (:uri ctx) ctx) (deterministic-ident ctx)))

(defn- field-at-path [path ordered-fields]
  (loop [[segment & rest] path
         fields ordered-fields]
    (cond
      (#{:head :body} segment) (recur rest fields)
      (context/find-element-segment? segment) (let [field (get fields segment)]
                                                (if (seq rest)
                                                  (recur rest (::field/children field))
                                                  field))
      :else (let [field (first (filter #(= (::field/path-segment %) segment) fields))]
              (if (seq rest)
                (recur rest (::field/children field))
                field)))))

(defn auto-formula [ctx link]
  (-> (memoized-safe-read-edn-string (str "[" (:link/path link) "]"))
      (either/branch
        (fn [e]
          (timbre/error e)
          nil)
        (fn [path]
          (if (:link/create? link)
            (if (some #{:head} path)
              "hypercrud.browser.auto-link-formula/auto-entity-from-stage"
              (when (::field/data-has-id? (field-at-path path @(:hypercrud.browser/fields ctx))) ; todo better reactivity
                "hypercrud.browser.auto-link-formula/auto-entity"))
            (when (::field/data-has-id? (field-at-path path @(:hypercrud.browser/fields ctx)))
              "(comp deref :hypercrud.browser/data)"))))))
