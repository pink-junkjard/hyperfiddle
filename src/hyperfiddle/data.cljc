(ns hyperfiddle.data
  (:require
    [cats.core :refer [>>=]]
    [cats.monad.either :refer [left right]]
    [contrib.ct :refer [unwrap]]
    [contrib.reactive :as r]
    [cuerdas.core :as str]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hyperfiddle.fiddle]))


(defn form-with-naked-legacy "Field is invoked as fn"       ; because it unifies with request fn side
  [ctx]                                                     ; f-field :: (relative-path ctx) => Any
  (let [paths (->> (r/fmap ::field/children (:hypercrud.browser/field ctx))
                   (r/unsequence ::field/path-segment)
                   (mapcat (fn [[field path-segment]]
                             (cond->> [[path-segment]]
                               (context/find-element-segment? path-segment) ; this only happens once at the top for relation queries
                               (into (->> @(r/fmap->> field ::field/children (map ::field/path-segment))
                                          (mapv (fn [child-segment]
                                                  [path-segment child-segment]))))))))]
    (if-not (= :relation (-> ctx :hypercrud.browser/field deref ::field/level)) ; omit relation-[]
      (conj (vec paths) [])                                 ; entity-[]
      (vec paths))))

(defn select-all [ctx ?corcs]
  (@(:hypercrud.browser/link-index ctx) ?corcs))

(defn ^:export select-all-here "reactive" ; collapses if eav is part of corcs
  [ctx & [?corcs]]
  (assert (= (second (:hypercrud.browser/eav ctx)) (->> (:hypercrud.browser/path ctx) (drop-while int?) last)))
  (-> (contrib.data/xorxs ?corcs #{})
      (conj (second (:hypercrud.browser/eav ctx)))
      (->> (select-all ctx))))

(defn validate-one+ [class links]
  (let [n (count links)]
    (condp = n
      1 (right (first links))
      0 (left (str/format "no match for class: %s" (pr-str class)))
      (left (str/format "Too many (%s) links matched for class: %s" n (pr-str class))))))

(defn ^:export select+ [ctx & [?corcs]]                     ; Right[Reaction[Link]], Left[String]
  (->> (@(:hypercrud.browser/link-index ctx) ?corcs)
       (validate-one+ ?corcs)))

(defn ^:export select-here+ [ctx & [?corcs]]
  {:pre [ctx]}
  (select+ ctx (-> (contrib.data/xorxs ?corcs #{})
                   (conj (second (:hypercrud.browser/eav ctx))))))

(defn ^:export browse+ "Navigate a link by hydrating its context accounting for dependencies in scope.
  returns Either[Loading|Error,ctx]."
  [ctx & [?corcs]]
  ; No focusing, can select from root, and data-from-link manufactures a new context
  (>>= (select+ ctx ?corcs)
       #(base/data-from-link @% ctx)))

(defn ^:export select-here "reactive" [ctx & [?corcs]]
  (->> (select-here+ ctx ?corcs) (unwrap (constantly nil))))

(defn ^:export select "reactive" [ctx & [?corcs]]
  (->> (select+ ctx ?corcs) (unwrap (constantly nil))))

(defn ^:export browse [ctx & [?corcs]]
  (->> (browse+ ctx ?corcs) (unwrap (constantly nil))))
