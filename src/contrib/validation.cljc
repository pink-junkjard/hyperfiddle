(ns contrib.validation
  (:require
    [clojure.core.match :refer [match #?(:cljs match*)]]
    [clojure.spec.alpha :as s]
    [contrib.spec :refer [describe]]))

(defn semantic-in [row-keyfn value in]  ; ({:id 1234 :some :value}  {:id 4321 :some :value})
  ; Trace the :in through the :val
  ; if the thing has an identity, use that instead
  ; accumulate the new path
  (reduce (fn [in k]
            (let [value (get-in value (conj in k))
                  k (if (int? k)
                      (or (row-keyfn value) k)
                      k)]
              (conj in k)))
          []
          in))

(defn explained-for-view
  "Efficient views need a stable keyfn to descend into rows. Canonicalize the spec problem :in's
  to align with the view's keyfn."
  [row-keyfn {::s/keys [value] :as e}]                      ; Need entire explained struct, problems alone is not enough
  (update e ::s/problems (fn [problems]
                           (map #(update % :in (partial semantic-in row-keyfn value)) problems))))

(defn form-cell-problem [{:keys [in val pred via] :as problem}]
  (let [pred (s/abbrev pred)
        validation-errors (-> via last describe :reason)]
    (#?(:clj match :cljs match*)
      [pred]

      ; Spec reports missing keys at the parent :in, path it at the child instead
      [(['contains? '% k] :seq)] [(conj in k) validation-errors]

      [_] [in validation-errors])))

(defn form-validation-hints
  "UI for forms need to validate at the cell granularity, not the record, so certain
  types of problems need to be re-pathed from entity to attribute.

      ([[2 :foo/bar] :contrib.validation/missing])"
  [problems]                                                ; destructure ::s/problems at call site
  ; Don't collect here, they get filtered down later
  (map form-cell-problem problems))

(defn validate [?spec value keyfn]
  {:pre [keyfn]}
  (when ?spec                                               ; just make this easy, specs are always sparse
    (when-let [explain (s/explain-data ?spec value)]
      (-> explain
          ;(assoc ::s/message msg)
          (->> (explained-for-view keyfn))
          ::s/problems
          form-validation-hints))))