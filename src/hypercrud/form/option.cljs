(ns hypercrud.form.option
  (:require [cats.core :as cats]
            [cats.monad.either :refer-macros [try-either]]))

(defn default-label-renderer [v ctx]
  (cond
    (instance? cljs.core/Keyword v) (name v)
    :else (str v))
  #_(condp (fn [x c] (instance? c x)) v
      cljs.core/Keyword (name v)
      (str v)))

(defn build-label [ordered-fes relation param-ctx]
  (->> ordered-fes
       (mapcat (fn [fe]
                 (->> (-> fe :find-element/form :form/field)
                      (mapv (fn [{:keys [:field/attribute]}]
                              ; Custom label renderers? Can't use the attribute renderer, since that
                              ; is how we are in a select options in the first place.
                              (let [value (get-in relation [(:find-element/name fe) attribute])
                                    renderer (or (-> param-ctx :fields attribute :label-renderer) default-label-renderer)]
                                (try-either (renderer value param-ctx))))))))
       (cats/sequence)
       (cats/fmap (fn [labels]
                    (->> labels
                         (interpose ", ")
                         (apply str))))))
