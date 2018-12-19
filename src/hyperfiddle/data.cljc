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
    [hypercrud.browser.link :as link]))


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

(letfn [(link-matches-class? [?corcs page-fiddle-ident link]
          ; What we've got fully matches what we asked for
          (clojure.set/superset?
            (-> (set (:link/class link))
                (conj (some-> link :link/fiddle :fiddle/ident))
                (conj (or (some-> link :link/path link/read-path last)
                          page-fiddle-ident))               ; links with no path, e.g. FindScalar and FindColl, are named by fiddle
                (conj (some-> link :link/tx-fn (subs 1) keyword)))
            (contrib.data/xorxs ?corcs)))]
  (defn select-all-r "List[Link]. Find the closest match."
    ([ctx] {:pre [ctx]}
     (r/fmap->> (:hypercrud.browser/fiddle ctx)
                :fiddle/links
                (filter (r/comp (r/partial hypercrud.browser.context/deps-satisfied? ctx) link/read-path :link/path))))
    ([ctx ?corcs]
     (r/fmap->> (select-all-r ctx)
                (filter (r/partial link-matches-class? ?corcs @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/ident])))))))

(defn ^:export select-all-here "List[Link]. Find the closest match."
  [ctx & [?corcs]]
  (r/fmap->> (select-all-r ctx ?corcs)
             (filter (r/partial link/same-path-as? (:hypercrud.browser/path ctx)))))

(defn validate-one+ [class links]
  (let [n (count links)]
    (condp = n
      1 (right (first links))
      0 (left (str/format "no match for class: %s" (pr-str class)))
      (left (str/format "Too many (%s) links matched for class: %s" n (pr-str class))))))

(defn ^:export select-here+ [ctx & [?corcs]]
  {:pre [ctx]}
  (->> (r/fmap->> (select-all-r ctx ?corcs)
                  (filter (r/partial link/same-path-as? (:hypercrud.browser/path ctx)))
                  (validate-one+ ?corcs))
       r/apply-inner-r
       deref))

(defn ^:export select+ [ctx & [?corcs]]                     ; Right[Reaction[Link]], Left[String]
  (->> (r/fmap->> (select-all-r ctx ?corcs)
                  (validate-one+ ?corcs))
       r/apply-inner-r
       deref))

(defn ^:export browse+ "Navigate a link by hydrating its context accounting for dependencies in scope.
  returns Either[Loading|Error,ctx]."
  [ctx & [?corcs]]
  ; No focusing, can select from root, and data-from-link manufactures a new context
  (>>= (select+ ctx ?corcs)
       #(base/data-from-link @% ctx)))

(defn ^:export select-here [ctx & [?corcs]]
  (->> (select-here+ ctx ?corcs) (unwrap (constantly nil))))

(defn ^:export select [ctx & [?corcs]]
  (->> (select+ ctx ?corcs) (unwrap (constantly nil))))

(defn ^:export browse [ctx & [?corcs]]
  (->> (browse+ ctx ?corcs) (unwrap (constantly nil))))
