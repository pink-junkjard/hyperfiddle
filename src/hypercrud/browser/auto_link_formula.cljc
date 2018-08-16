(ns hypercrud.browser.auto-link-formula                     ; namespace is public export runtime
  (:require [cats.monad.either :as either]
            [contrib.data :refer [abs-normalized]]
            [contrib.reactive :as r]
            [contrib.string :refer [memoized-safe-read-edn-string]]
            [hypercrud.browser.field :as field]
            [hypercrud.browser.context :as context]
            [hypercrud.types.ThinEntity :refer [->ThinEntity]]
            [hypercrud.util.branch :as branch]
            [hyperfiddle.domain :as domain]
            [hyperfiddle.runtime :as runtime]
            [taoensso.timbre :as timbre]))


(defn ^:export auto-entity-from-stage [ctx]
  ; This returns a new value each time the transaction changes - can't call it again later.
  ; So tx-fns must inspect the modal-route, they can't re-create the dbid.
  (assert (:uri ctx) "no uri in dynamic scope (If it can't be inferred, add as bindings)")
  (let [branch-val (branch/branch-val (:uri ctx) (:branch ctx) @(runtime/state (:peer ctx) [:stage]))
        id (-> branch-val hash abs-normalized - str)]
    (->ThinEntity (domain/uri->dbname (:uri ctx) (:hypercrud.browser/domain ctx)) id)))

; todo there are collisions when two links share the same 'location'
(letfn [(hash-data [ctx]
          (when-let [data (:hypercrud.browser/data ctx)]
            (case @(r/fmap ::field/cardinality (:hypercrud.browser/field ctx))
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
  (->ThinEntity (domain/uri->dbname (:uri ctx) (:hypercrud.browser/domain ctx)) (deterministic-ident ctx)))

(defn- field-at-path [path field]
  (if path
    (let [[segment & rest] path]
      (if (#{:head :body} segment)                          ; f'ed :head and :body
        (field-at-path rest field)
        (let [field (->> (::field/children field)
                         (filter #(= (::field/path-segment %) segment))
                         first)]
          (if (seq rest)
            (field-at-path rest field)
            field))))
    (when (nil? (::field/path-segment field))
      field)))

(defn auto-formula [ctx link]                               ; f'ed :head and :body
  (-> (memoized-safe-read-edn-string (str "[" (:link/path link) "]"))
      (either/branch
        (fn [e]
          (timbre/error e)
          nil)
        (fn [path]
          (if (:link/create? link)
            (if (some #{:head} path)                        ; f'ed :head and :body - dispatch on :rel instead
              "hypercrud.browser.auto-link-formula/auto-entity-from-stage"
              (when (->> (:hypercrud.browser/field ctx)
                         (r/fmap (r/partial field-at-path path))
                         (r/fmap ::field/data-has-id?)
                         deref)
                "hypercrud.browser.auto-link-formula/auto-entity"))
            (when path
              (->> (:hypercrud.browser/field ctx)
                   (r/fmap (r/partial field-at-path path))
                   (r/fmap ::field/data-has-id?)
                   deref)
              "(comp deref :hypercrud.browser/data)"))))))
