(ns hyperfiddle.reducers
  (:require
    [clojure.spec.alpha :as s]
    [contrib.data :refer [map-values]]
    [contrib.datomic-tx :as tx]
    [contrib.pprint :refer [pprint-str]]
    [hypercrud.types.Err :refer [->Err]]
    [hyperfiddle.branch :as branch]
    [hyperfiddle.route]                                     ; spec validation
    [hyperfiddle.state :as state]))


(defn- serializable-error [e]
  ; need errors to be serializable, so crapily pr-str
  (let [?message #?(:clj (.getMessage e) :cljs (ex-message e))]
    (cond
      (string? e) e
      ?message (assoc (->Err (str ?message)) :data (pprint-str (ex-data e)))
      :else (pr-str e))))

(defn fatal-error-reducer [error action & args]
  (case action
    :set-global-basis nil
    :set-error (serializable-error (first args))
    error))

(defn global-basis-reducer [global-basis action & args]
  (case action
    :hydrate!-route-success (let [[branch attr-renderers project ptm schemas tempid-lookups new-local-basis] args]
                              (if (nil? branch)
                                (map-values (fn [sub-basis]
                                              (reduce-kv (fn [sub-basis uri t]
                                                           (if (contains? sub-basis uri)
                                                             (assoc sub-basis uri t)
                                                             sub-basis))
                                                         sub-basis
                                                         new-local-basis))
                                            global-basis)
                                global-basis))
    :set-global-basis (first args)
    global-basis))

(defn partitions-reducer [partitions action & args]
  (let [with (fn [partition dbname tx]
               (let [schema (get-in partition [:schemas dbname])]
                 (update-in partition [:stage dbname] (partial tx/into-tx schema) tx)))]
    (->> (case action
           :transact!-success
           (let [[branch-id dbnames] args]
             (-> partitions
                 (assoc-in [branch-id :hydrate-id] "hack; dont flicker while page rebuilds")
                 (update-in [branch-id :stage] #(apply dissoc % dbnames))))

           :create-partition (let [[branch-id] args
                                   parent-branch-id (branch/parent-branch-id branch-id)]
                               (-> partitions
                                   (update-in [parent-branch-id ::branch/children] conj branch-id)
                                   (assoc branch-id {})))

           :discard-partition (let [[branch-id] args
                                    parent-branch-id (branch/parent-branch-id branch-id)]
                                (-> partitions
                                    (dissoc branch-id)
                                    (update-in [parent-branch-id ::branch/children] disj branch-id)))

           :partition-basis (let [[branch local-basis] args]
                              (assoc-in partitions [branch :local-basis] local-basis))

           :partition-route (let [[branch route] args]
                              (assert (some? (get partitions branch)) "Must create-partition before setting route")
                              (assoc-in partitions [branch :route] route))

           :with (let [[branch dbname tx] args]
                   (update partitions branch with dbname tx))

           :merge (let [[branch] args
                        parent-branch (branch/parent-branch-id branch)]
                    (-> (reduce (fn [partitions [dbname tx]]
                                  (update partitions parent-branch with dbname tx))
                                partitions
                                (get-in partitions [branch :stage]))
                        (update branch dissoc :stage)))

           :hydrate!-start (let [[branch] args]
                             (update partitions branch
                                     (fn [partition]
                                       (assoc partition
                                         :hydrate-id
                                         (hash (select-keys partition [:route :stage :local-basis]))))))

           :hydrate!-success (let [[branch ptm tempid-lookups] args]
                               (update partitions branch
                                       (fn [partition]
                                         (-> partition
                                             (dissoc :error :hydrate-id)
                                             (assoc :ptm ptm
                                                    :tempid-lookups tempid-lookups)))))

           :hydrate!-shorted (let [[branch] args]
                               (update partitions branch dissoc :hydrate-id))

           :hydrate!-route-success (let [[branch attr-renderers project ptm schemas tempid-lookups new-basis] args]
                                     (update partitions branch
                                             (fn [partition]
                                               (-> partition
                                                   (dissoc :error :hydrate-id)
                                                   (assoc
                                                     :attr-renderers attr-renderers
                                                     :local-basis new-basis
                                                     :project project
                                                     :ptm ptm
                                                     :schemas schemas
                                                     :tempid-lookups tempid-lookups)))))

           :partition-error (let [[branch error] args]
                              (update partitions branch
                                      (fn [partition]
                                        (-> partition
                                            (dissoc :hydrate-id)
                                            (assoc :error (serializable-error error))))))

           :open-popover (let [[branch popover-id] args]
                           (update-in partitions [branch :popovers] conj popover-id))

           :close-popover (let [[branch popover-id] args]
                            (update-in partitions [branch :popovers] disj popover-id))

           :close-all-popovers (let [[branch] args] (update partitions branch dissoc :popovers))

           :reset-stage-branch (let [[branch v] args]
                                 (assoc-in partitions [branch :stage] v))

           :reset-stage-db (let [[branch dbname tx] args]
                             (assoc-in partitions [branch :stage dbname] tx))

           (or partitions {}))
         (map-values (fn [partition]
                       ; apply defaults
                       (->> {:hydrate-id identity
                             :popovers #(or % #{})
                             ::branch/children #(or % #{})

                             ; data needed to hydrate a partition
                             :route #(some->> % (s/assert :hyperfiddle/route))
                             :stage (fn [multi-color-tx]
                                      (->> multi-color-tx
                                           (remove (fn [[dbname tx]] (empty? tx)))
                                           (into {})))
                             :local-basis identity

                             ; response data of hydrating a partition
                             :attr-renderers identity
                             :error identity
                             :project identity
                             :ptm identity
                             :schemas identity
                             :tempid-lookups identity}
                            (reduce-kv update partition)))))))

(defn auto-transact-reducer [auto-tx action & args]
  (case action
    :set-auto-transact (first args)
    :toggle-auto-transact (let [[dbname] args]
                            (update auto-tx dbname not))
    (or auto-tx {})))

(defn user-id-reducer [user-id action & args]
  (case action
    :set-user-id (first args)
    user-id))

(def reducer-map {:hyperfiddle.runtime/fatal-error fatal-error-reducer
                  :hyperfiddle.runtime/global-basis global-basis-reducer
                  :hyperfiddle.runtime/partitions partitions-reducer
                  :hyperfiddle.runtime/auto-transact auto-transact-reducer
                  :hyperfiddle.runtime/user-id user-id-reducer})

(def root-reducer (state/combine-reducers reducer-map))
