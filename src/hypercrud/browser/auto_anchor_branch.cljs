(ns hypercrud.browser.auto-anchor-branch)


(defn auto-branch [anchor param-ctx]
  (let [{r :anchor/repeating? e :anchor/find-element a :anchor/attribute} anchor]
    (cond

      ; only care about managed cases

      ; attr create
      (and (not r) a)
      (-> (str (-> param-ctx :entity :db/id :id) "."
               (-> param-ctx :attribute :attribute/ident) "."
               (case (-> ((:schema param-ctx) (-> param-ctx :attribute :attribute/ident)) :attribute/cardinality :db/ident)
                 :db.cardinality/one nil
                 :db.cardinality/many (hash (into #{} (mapv :db/id (:value param-ctx))))))
          hash js/Math.abs - str)

      ;entity create
      (and (not r) (not a))
      (-> (str (-> param-ctx :entity :db/id :id) "." ".")
          hash js/Math.abs - str)

      :else nil)))
