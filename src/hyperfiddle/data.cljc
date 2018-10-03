(ns hyperfiddle.data
  (:require
    [cats.core :refer [fmap >>=]]
    [cats.monad.either :refer [left right]]
    [contrib.ct :refer [unwrap]]
    [contrib.reactive :as r]
    [cuerdas.core :as str]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.link :as link]
    [hypercrud.browser.context :as context]
    [hyperfiddle.tempid :refer [stable-relation-key]]))


(defn row-keyfn [ctx row]
  {:pre [(not (r/reactive? row))]}
  ; This keyfn is very tricky, read https://github.com/hyperfiddle/hyperfiddle/issues/341
  (-> (if (or (vector? row) (seq? row))                     ; todo should probably inspect fields instead of seq
        (map (partial stable-relation-key ctx) row)
        (stable-relation-key ctx row))
      hash))

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

(defn deps-satisfied? "Links in this :body strata" [ctx target-path]
  (let [this-path (:hypercrud.browser/path ctx)
        common-path (contrib.data/ancestry-common this-path target-path)
        common-ctx (context/refocus ctx common-path)]
    (loop [field (:hypercrud.browser/field common-ctx)
           [segment & xs] (contrib.data/ancestry-divergence target-path common-path)]
      (if-not segment
        true                                                ; finished
        (let [child-field (r/fmap->> field (context/find-child-field segment (:hypercrud.browser/schemas ctx)))]
          (case @(r/fmap ::field/cardinality child-field)
            :db.cardinality/one (recur child-field xs)
            :db.cardinality/many false
            false) #_"Nonsensical path - probably invalid links for this query, maybe they just changed the query and the links broke")))))

(defn link-path-floor "Find the shortest path that has equal dimension" [ctx path]
  (loop [ctx (context/refocus ctx path)]                    ; we know we're at least satisfied so this is safe
    (if-let [parent-ctx (:hypercrud.browser/parent ctx)]    ; walk it up and see if the dimension changes
      (case (::field/cardinality @(:hypercrud.browser/field parent-ctx))
        :db.cardinality/one (recur parent-ctx)              ; Next one is good, keep going
        :db.cardinality/many ctx)                           ; Next one is one too far, so we're done
      ctx)))                                                ; Empty path is the shortest path

(defn deps-over-satisfied? "satisfied but not over-satisfied" [ctx link-path]
  (let [this-path (:hypercrud.browser/path ctx)]
    (not= this-path (:hypercrud.browser/path (link-path-floor ctx link-path)))))

(defn select-all-r "List[Link]. Find the closest match."
  ([ctx] {:pre [ctx]}
   (r/fmap->> (:hypercrud.browser/fiddle ctx)
              :fiddle/links
              (filter (r/comp (r/partial deps-satisfied? ctx) link/read-path :link/path))))
  ([ctx rel] {:pre [ctx rel]}
   (r/fmap->> (select-all-r ctx)
              (filter (r/comp (r/partial = rel) :link/rel))))
  ([ctx rel ?corcs]
   (r/fmap->> (select-all-r ctx rel)
              (filter (r/comp (r/partial r/last-arg-first clojure.set/superset? (contrib.data/xorxs ?corcs))
                              ;  add more stuff here
                              set :link/class)))))

(defn ^:export select-all "List[Link]. Find the closest match." [& args] @(apply select-all-r args))

(defn validate-one+ [rel class links]
  (let [n (count links)]
    (condp = n
      1 (right (first links))
      0 (left (str/format "no match for rel: %s class: %s" (pr-str rel) (pr-str class)))
      (left (str/format "Too many links matched (%s) for rel: %s class: %s" n (pr-str rel) (pr-str class))))))

(defn ^:export select-here+ [ctx rel & [?corcs]]
  {:pre [ctx]}
  (->> (r/fmap->> (select-all-r ctx rel ?corcs)
                  (filter (r/comp (r/partial = (:hypercrud.browser/path ctx)) link/read-path :link/path))
                  (validate-one+ rel ?corcs))
       r/apply-inner-r
       deref))

(defn ^:export select+ [ctx rel & [?corcs]]                 ; Right[Reaction[Link]], Left[String]
  (->> (r/fmap->> (select-all-r ctx rel ?corcs)
                  (validate-one+ rel ?corcs))
       r/apply-inner-r
       deref))

(defn ^:export browse+ "Navigate a link by hydrating its context accounting for dependencies in scope.
  returns Either[Loading|Error,ctx]."
  [ctx rel & [?corcs]]
  ; No focusing, can select from root, and data-from-link manufactures a new context
  (>>= (select+ ctx rel ?corcs)
       #(base/data-from-link @% ctx)))

(defn ^:export select-here [ctx rel & [?corcs]]
  (->> (select-here+ ctx rel ?corcs) (unwrap (constantly nil))))

(defn ^:export select [ctx rel & [?corcs]]
  (->> (select+ ctx rel ?corcs) (unwrap (constantly nil))))

(defn ^:export browse [ctx rel & [?corcs]]
  (->> (browse+ ctx rel ?corcs) (unwrap (constantly nil))))
