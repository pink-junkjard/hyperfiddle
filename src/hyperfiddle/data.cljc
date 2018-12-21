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

(defn select-many "All links we can reach (for this entire dimension)"
  [ctx ?corcs]
  {:pre [#_(:hypercrud.browser/qfind ctx)                   ; :blank ?
         #_(:hypercrud.browser/element ctx)                 ; database color
         (:hypercrud.browser/link-index ctx)
         (:hypercrud.browser/eav ctx)]                      ; it can be Reaction of [nil nil nil]
   :post [(r/reactive? %)]}
  (context/links-in-dimension-r                                ; hidden deref
    (:hypercrud.browser/link-index ctx)
    (:hypercrud.browser/element ctx)                        ; optional?
    (:hypercrud.browser/path ctx)                           ; optional?
    (contrib.data/xorxs ?corcs #{})))

(defn ^:export select-many-here "reactive" ; collapses if eav is part of corcs
  [ctx & [?corcs]]
  {:pre [(:hypercrud.browser/eav ctx)]
   :post [(r/reactive? %)]}
  (-> (contrib.data/xorxs ?corcs #{})
      (conj (second @(:hypercrud.browser/eav ctx)))
      (->> (select-many ctx))))

(defn validate-one+r [corcs r-links]                        ; Right[Reaction] or Left[Error] - broken, error should react too
  (let [n @(r/fmap count r-links)]
    (condp = n
      1 (right (r/fmap first r-links))
      0 (left (str/format "no match for class: %s" (pr-str corcs)))
      (left (str/format "Too many (%s) links matched for criteria: %s" n (pr-str corcs))))))

(defn ^:export select+ [ctx & [?corcs]]                     ; Right[Reaction[Link]] or Left[String]
  (->> (validate-one+r ?corcs (select-many ctx ?corcs))
       #_#_r/apply-inner-r deref))

(defn ^:export select-here+ [ctx & [?corcs]]
  {:pre [ctx]
   :post [#_(r/reactive? %)]}
  (->> (validate-one+r ?corcs (select-many-here ctx ?corcs))
       #_#_r/apply-inner-r deref))

(defn ^:export browse+ "Navigate a link by hydrating its context accounting for dependencies in scope.
  returns Either[Loading|Error,ctx]."
  [ctx & [?corcs]]
  ; No focusing, can select from root, and data-from-link manufactures a new context
  (>>= (select+ ctx ?corcs)
       #(base/data-from-link @% ctx)))

(defn ^:export select-here "reactive" [ctx & [?corcs]]
  {:post [(or #_(nil? %) (r/reactive? %))]}
  (->> (select-here+ ctx ?corcs) (unwrap (constantly (r/track identity nil)))))

(defn ^:export select "reactive" [ctx & [?corcs]]
  {:pre [(context/summon-schemas-grouped-by-dbname ctx)]
   :post [(r/reactive? %)]}
  (->> (select+ ctx ?corcs) (unwrap (constantly (r/track identity nil)))))

(defn ^:export browse [ctx & [?corcs]]
  {:post [(r/reactive? %)]}
  (->> (browse+ ctx ?corcs) (unwrap (constantly (r/track identity nil)))))
