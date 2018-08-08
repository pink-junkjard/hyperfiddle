(ns hyperfiddle.ui.api
  (:require
    [cats.core :as cats]
    [contrib.data :refer [unwrap]]
    [contrib.reactive :as r]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.link :as link]
    [hyperfiddle.data :as data]))


(declare api-data)
(declare with-result)

(defn recurse-from-route [route ctx]
  (->> (base/data-from-route route ctx)
       (cats/fmap api-data)
       ; todo cannot swallow this error
       (unwrap)))

(defn recurse-from-link [link ctx]
  (->> (base/data-from-link link ctx)
       (cats/fmap api-data)
       ; todo cannot swallow this error
       (unwrap)))

(defn head-field [relative-path ctx]
  (let [ctx (context/focus ctx (cons :head relative-path))] ; todo :head links should fall out with link/class
    (->> @(:hypercrud.browser/links ctx)
         (filter (partial link/same-path-as? (:hypercrud.browser/path ctx)))
         (map #(recurse-from-link % ctx))
         (apply merge))))

(defn body-field [relative-path ctx]
  (let [ctx (context/focus ctx relative-path)]
    (->> @(:hypercrud.browser/links ctx)
         (filter (partial link/same-path-as? (:hypercrud.browser/path ctx)))
         (map #(recurse-from-link % ctx))
         (apply merge)
         (into (let [child-fields? (not (some->> (:hypercrud.browser/fields ctx) (r/fmap nil?) deref))]
                 (if (and child-fields? (context/attribute-segment? (last (:hypercrud.browser/path ctx)))) ; ignore relation and fe fields
                   (with-result ctx)
                   {}))))))

(defn with-result [ctx]
  (condp = (:hypercrud.browser/data-cardinality ctx)
    :db.cardinality/one (->> ctx
                             (data/form (fn [path ctx]
                                          (merge (head-field path (context/focus ctx [:head]))
                                                 (body-field path (context/focus ctx [:body])))))
                             (apply merge))
    :db.cardinality/many (merge (->> (data/form head-field (context/focus ctx [:head]))
                                     (apply merge))
                                (->> (r/unsequence (:hypercrud.browser/data ctx)) ; the request side does NOT need the cursors to be equiv between loops
                                     (map (fn [[relation i]]
                                            (->> (context/body ctx relation)
                                                 (data/form body-field)
                                                 (apply merge))))
                                     (apply merge)))
    ; blank fiddles
    {}))

(defn- filter-inline [links] (filter :link/render-inline? links))
(defn- remove-managed [links] (remove :link/managed? links))

(defn api-data [ctx]
  ; at this point we only care about inline links
  ; also no popovers can be opened, so remove managed
  (let [ctx (update ctx :hypercrud.browser/links (partial r/fmap (r/comp remove-managed filter-inline)))]
    (merge (with-result ctx)
           (->> (link/links-at (:hypercrud.browser/path ctx) (:hypercrud.browser/links ctx))                     ; todo reactivity
                (map #(recurse-from-link % ctx))
                (apply merge))
           (when @(r/fmap :fiddle/hydrate-result-as-fiddle (:hypercrud.browser/fiddle ctx))
             ; This only makes sense on :fiddle/type :query because it has arbitrary arguments
             ; EntityRequest args are too structured.
             (let [[_ [inner-fiddle & inner-args]] (:route ctx)]
               (recurse-from-route [inner-fiddle (vec inner-args)] ctx)))
           {(:route ctx) @(:hypercrud.browser/result ctx)})))
