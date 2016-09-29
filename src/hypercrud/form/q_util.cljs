(ns hypercrud.form.q-util)


(defn parse-holes [q]
  (let [last-kw (atom nil)
        f (fn [x]
            (if (keyword? x) (reset! last-kw x))
            @last-kw)]
    (->> (partition-by f q)
         (filter #(= :in (first %)))
         first
         (drop 2)
         (map str))))


(defn build-params [q param-ctx]
  (->> (parse-holes q)
       (map (fn [hole-name]
              (let [value (get param-ctx hole-name)]
                (assert (not= nil value) (str "Empty parameter for " hole-name " in " q))
                value)))))


(defn build-query [query-name q param-ctx pull-exp]
  (let [params (build-params q param-ctx)
        value [q params pull-exp]
        query-name (or query-name (hash value))]
    {query-name value}))
