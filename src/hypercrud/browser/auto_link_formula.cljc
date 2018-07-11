(ns hypercrud.browser.auto-link-formula                     ; namespace is public export runtime
  (:require [cats.monad.either :as either]
            [contrib.data :refer [abs-normalized]]
            [contrib.reactive :as r]
            [contrib.string :refer [memoized-safe-read-edn-string]]
            [hypercrud.browser.dbname :as dbname]
            [hypercrud.browser.find-element :as field]
            [hypercrud.browser.context :as context]
            [hypercrud.types.Entity :refer [#?(:cljs Entity)]]
            [hypercrud.types.ThinEntity :refer [->ThinEntity]]
            [hypercrud.util.branch :as branch]
            [hyperfiddle.runtime :as runtime]
            [taoensso.timbre :as timbre])
  #?(:clj
     (:import (hypercrud.types.Entity Entity))))


(defn ^:export auto-entity-from-stage [ctx]
  ; This returns a new value each time the transaction changes - can't call it again later.
  ; So tx-fns must inspect the modal-route, they can't re-create the dbid.
  (assert (:uri ctx) "no uri in dynamic scope (If it can't be inferred, add as bindings)")
  (let [branch-val (branch/branch-val (:uri ctx) (:branch ctx) @(runtime/state (:peer ctx) [:stage]))
        id (-> branch-val hash abs-normalized - str)]
    (->ThinEntity (dbname/uri->dbname (:uri ctx) ctx) id)))

; todo there are collisions when two links share the same 'location'
(defn deterministic-ident [ctx]
  (-> (str (:hypercrud.browser/path ctx) "."
           (if-let [data (:hypercrud.browser/data ctx)]
             (case (:hypercrud.browser/data-cardinality ctx)
               :db.cardinality/one @(r/fmap :db/id data)
               :db.cardinality/many (hash (into #{} @(r/fmap (partial mapv :db/id) data))) ; todo scalar
               nil nil #_":db/id has a faked attribute with no cardinality, need more thought to make elegant")))
      hash abs-normalized - str))

(let [get-uri (fn [default cell-data]
                (if (instance? Entity cell-data)
                  (.-uri cell-data)
                  default))]
  (defn auto-entity [ctx]
    (->ThinEntity (dbname/uri->dbname (:uri ctx) ctx) (deterministic-ident ctx))))

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

(defn -auto-formula-impl [ctx path & {:keys [create?] :or {create? false}}] ; todo clean up these args, remove ctx
  (if create?
    (if (empty? path)
      "hypercrud.browser.auto-link-formula/auto-entity-from-stage"
      (when (::field/data-has-id? (field-at-path path @(:hypercrud.browser/fields ctx))) ; todo better reactivity
        ; todo d? false used to be "hypercrud.browser.auto-link-formula/auto-entity-from-stage"
        "hypercrud.browser.auto-link-formula/auto-entity"))
    (when (::field/data-has-id? (field-at-path path @(:hypercrud.browser/fields ctx)))
      "(comp deref :hypercrud.browser/data)"))

  #_(case {:fe (not (nil? (first path))) :c? create?}
      {:fe true :c? false} (if (nil? (second path))
                             (when @(r/cursor (:hypercrud.browser/ordered-fes ctx) [(first path) :entity])
                               "(comp deref :cell-data)")
                             (let [dbname (str @(r/cursor (:hypercrud.browser/ordered-fes ctx) [(first path) :source-symbol]))]
                               (case @(r/cursor (:hypercrud.browser/schemas ctx) [dbname (second path) :db/cardinality :db/ident])
                                 :db.cardinality/one "(comp deref :value)"

                                 ; "find children of parent entity at attr". See base ->EntityRequest
                                 :db.cardinality/many (when @(r/cursor (:hypercrud.browser/ordered-fes ctx) [(first path) :entity])
                                                        "(juxt (comp deref :cell-data) :hypercrud.browser/attribute)")

                                 ; the attribute doesnt exist
                                 nil "(comp deref :value)")))
      {:fe true :c? true} (when @(r/cursor (:hypercrud.browser/ordered-fes ctx) [(first path) :entity])
                            ; todo d? false used to be "hypercrud.browser.auto-link-formula/auto-entity-from-stage"
                            "hypercrud.browser.auto-link-formula/auto-entity")

      ; no fe = index or relation links
      {:fe false :c? false} nil
      {:fe false :c? true} (when (empty? path)
                             "hypercrud.browser.auto-link-formula/auto-entity-from-stage")))

(defn auto-formula [ctx link]
  (-> (memoized-safe-read-edn-string (str "[" (:link/path link) "]"))
      (either/branch
        (fn [e]
          (timbre/error e)
          nil)
        (fn [path]
          (-auto-formula-impl ctx path :create? (:link/create? link))))))
