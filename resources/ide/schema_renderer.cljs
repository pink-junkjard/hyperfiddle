(let [hide-datomic (contrib.reactive/atom true)
      hide-archived (contrib.reactive/atom true)
      needle (contrib.reactive/atom nil)
      db-attr? #(<= (:db/id %) 62)
      archived? #(cuerdas.core/starts-with? (namespace (:db/ident %)) "zzz") ; "zzz/" and "zzz.", we are inconsistent. It should be modeled and queried and never shown
      do-filter-reactive (fn [xs]                           ; perf sensitive
                           (as-> xs xs
                             (if @hide-datomic (remove db-attr? xs) xs)
                             (if @hide-archived (remove archived? xs) xs)
                             (if (contrib.string/blank->nil @needle)
                               (filter #(cuerdas.core/includes? (-> % :db/ident str) @needle) xs) xs)))]
  (fn [val ctx props]
    [:div props
     [hyperfiddle.ui/markdown (-> ctx :hypercrud.browser/fiddle deref :db/doc)]
     [:div [:label [:input {:type "checkbox" :checked @hide-datomic :on-change #(swap! hide-datomic not)}] " hide Datomic system attributes"]]
     [:div [:label [:input {:type "checkbox" :checked @hide-archived :on-change #(swap! hide-archived not)}] " hide Hyperfiddle archived attributes"]]
     [contrib.ui/text {:value @needle
                       :on-change #(do (reset! needle %))}
      {:placeholder ":task/title"}]
     (let [ctx (-> ctx
                   (update :hypercrud.browser/data (partial contrib.reactive/fmap do-filter-reactive))
                   (assoc :hyperfiddle.ui/layout :hyperfiddle.ui.layout/table))]
       [hyperfiddle.ui/table
        (fn [ctx]
          [(hyperfiddle.ui/field [:db/ident] ctx)
           (hyperfiddle.ui/field [:db/valueType] ctx #(hyperfiddle.ui.controls/string ((comp (fnil name :–) :db/ident) %) %2 %3) {:read-only true})
           (hyperfiddle.ui/field [:db/cardinality] ctx #(hyperfiddle.ui.controls/string ((comp (fnil name :–) :db/ident) %) %2 %3) {:read-only true})
           (hyperfiddle.ui/field [:db/unique] ctx #(hyperfiddle.ui.controls/string ((comp (fnil name :–) :db/ident) %) %2 %3) {:read-only true})
           (hyperfiddle.ui/field [:db/isComponent] ctx)
           (hyperfiddle.ui/field [:db/fulltext] ctx nil {:read-only true})
           (hyperfiddle.ui/field [:db/doc] ctx)])
        ctx])]))