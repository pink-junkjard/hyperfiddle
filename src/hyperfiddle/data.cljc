(ns hyperfiddle.data
  (:require
    [cats.core :refer [fmap]]
    [contrib.reactive :as r]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.link :as link]
    [hypercrud.browser.context :as context]))


(defn relation-keyfn [relation]
  {:pre [(not (r/reactive? relation))]}
  ; This keyfn is very tricky, read https://github.com/hyperfiddle/hyperfiddle/issues/341
  (hash (map #(or (:db/id %) %) relation)))

(letfn [(should-flatten? [m-field] (not (nil? (::field/source-symbol m-field))))]
  (defn form "Field is invoked as fn"                       ; because it unifies with request fn side
    [f-field ctx & [props]]                                 ; todo props shouldn't be passed through here
    (-> (->> (r/unsequence ::field/path-segment (:hypercrud.browser/fields ctx))
             (mapcat (fn [[m-field path-segment]]
                       ; this is silly why are we tossing the m-field data structure
                       (cond->> [(f-field [path-segment] ctx nil props)] ; entity links
                         @(r/fmap should-flatten? m-field)  ; this only happens once at the top for a fe pull expressions
                         (concat (->> (r/fmap ::field/children m-field)
                                      (r/unsequence ::field/path-segment)
                                      (mapv (fn [[m-child-field child-segment]]
                                              ; this is silly why are we tossing the m-child-field data structure
                                              (f-field [path-segment child-segment] ctx nil props)))))))))
        vec
        (cond->
          (not (and #_(some #(% (:hypercrud.browser/path ctx)) [empty? #{[:head] [:body]}]) ; (empty? path)    or path = [:head]    or path = [:body]
                    (->> (r/track link/links-at [:head] (:hypercrud.browser/links ctx))
                         (r/fmap empty?)
                         deref)
                    (->> (r/track link/links-at [:body] (:hypercrud.browser/links ctx))
                         (r/fmap empty?)
                         deref)))
          ; row/relation; omit if result-row & no links. eventually we should probably always display
          (conj (f-field [] ctx nil props)))

        ; this result can be directly inserted as children in a reagemnt component, CANNOT be a vector
        seq)))

(defn ^:export browse [rel relative-path ctx]
  ; context is not set for this call
  (let [ctx (context/focus ctx relative-path)]
    (->> (base/data-from-link @(r/track link/rel->link rel ctx) ctx)
         (fmap :hypercrud.browser/result)
         (fmap deref))))

(defn sort-fn [sort-col relations-val]
  (let [[path direction] @sort-col]
    (if path
      (sort-by #(get-in % path)
               (case direction
                 :asc #(compare %1 %2)
                 :desc #(compare %2 %1))
               relations-val)
      relations-val)))
