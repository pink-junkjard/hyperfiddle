(ns hyperfiddle.data
  (:require
    [cats.core :refer [fmap]]
    [cats.monad.either :refer [left right]]
    [contrib.reactive :as r]
    [cuerdas.core :as str]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.link :as link]
    [hypercrud.browser.context :as context]))


(defn relation-keyfn [relation]
  {:pre [(not (r/reactive? relation))]}
  ; This keyfn is very tricky, read https://github.com/hyperfiddle/hyperfiddle/issues/341
  (hash (map #(or (:db/id %) %) relation)))

(defn- relative-links-at [path-segments ctx]
  (let [path (cond
               (empty? (:hypercrud.browser/path ctx)) path-segments

               (contains? #{:head :body} (last (:hypercrud.browser/path ctx)))
               (-> (drop-last (:hypercrud.browser/path ctx))
                   vec
                   (concat path-segments))

               :else (concat (:hypercrud.browser/path ctx) path-segments))]
    (r/track link/links-at path (:hypercrud.browser/links ctx))))

(letfn [(should-flatten? [m-field] (not (nil? (::field/source-symbol m-field))))]
  (defn form "Field is invoked as fn"                       ; because it unifies with request fn side
    [f-field ctx]
    (-> (->> (r/unsequence ::field/path-segment (:hypercrud.browser/fields ctx))
             (mapcat (fn [[m-field path-segment]]
                       ; this is silly why are we tossing the m-field data structure
                       (cond-> []
                         @(r/fmap should-flatten? m-field)  ; this only happens once at the top for a fe pull expressions
                         (into (->> (r/fmap ::field/children m-field)
                                    (r/unsequence ::field/path-segment)
                                    (mapv (fn [[m-child-field child-segment]]
                                            ; this is silly why are we tossing the m-child-field data structure
                                            (f-field [path-segment child-segment] ctx)))))

                         (or (context/attribute-segment? path-segment)
                             (nil? (::field/source-symbol m-field))
                             (not @(r/fmap empty? (relative-links-at [:head path-segment] ctx)))
                             (not @(r/fmap empty? (relative-links-at [:body path-segment] ctx))))
                         (conj (f-field [path-segment] ctx))))))
        vec
        (cond->
          (or (not @(r/fmap empty? (relative-links-at [:head] ctx)))
              (not @(r/fmap empty? (relative-links-at [:body] ctx))))
          ; row/relation; omit if result-row & no links. eventually we should probably always display
          (conj (f-field [] ctx)))

        ; this result can be directly inserted as children in a reagemnt component, CANNOT be a vector
        seq)))

(defn ^:export browse "Hydrate a context, returns Either[Loading|Error,ctx]
  Navigate the fiddle graph, starting at the focus point, because there are dependencies in scope."
  [rel relative-path ctx]
  ; context is not set for this call
  (let [ctx (context/focus ctx relative-path)]
    (base/data-from-link @(r/track link/rel->link rel ctx) ctx)))

(defn select+ "get a link for browsing later" [ctx rel & [?class ?path]]
  (let [[x & xs] (link/select-all ctx rel ?class ?path)]
    (cond
      xs (left (str/format "Too many links matched for rel: %s class: %s path: %s" (pr-str rel) (pr-str ?class) ?path))
      x (right x)
      :else (left (str/format "no match for rel: %s class: %s path: %s" (pr-str rel) (pr-str ?class) ?path)))))
