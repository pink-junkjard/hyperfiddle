(ns hypercrud.ui.renderer)


(let [empty-string-to-nil (fn [s] (if (empty? s) nil s))]
  (defn renderer-for-attribute [attribute]
    (or (empty-string-to-nil (:attribute/renderer attribute))
        (empty-string-to-nil (-> attribute :attribute/hc-type :hc-type/renderer)))))
