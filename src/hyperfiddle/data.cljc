(ns hyperfiddle.data
  (:require
    [cats.core :refer [fmap >>=]]
    [cats.monad.either :refer [left right]]
    [contrib.reactive :as r]
    [cuerdas.core :as str]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.link :as link]
    [hypercrud.browser.context :as context]))


(defn row-keyfn [row]
  {:pre [(not (r/reactive? row))]}
  ; This keyfn is very tricky, read https://github.com/hyperfiddle/hyperfiddle/issues/341
  (-> (if (or (vector? row) (seq? row))                     ; todo should probably inspect fields instead of seq
        (map #(or (:db/id %) %) row)
        (or (:db/id row) row))
      hash))

(defn form-with-naked-legacy "Field is invoked as fn"       ; because it unifies with request fn side
  [ctx]                                                     ; f-field :: (relative-path ctx) => Any
  (let [paths (->> (r/fmap ::field/children (:hypercrud.browser/field ctx))
                   (r/unsequence ::field/path-segment)
                   (mapcat (fn [[field path-segment]]
                             (cond->> [[path-segment]]
                               (context/find-element-segment? path-segment) ; this only happens once at the top for relation queries
                               (into (->> (r/fmap ::field/children field)
                                          (r/unsequence ::field/path-segment)
                                          (mapv (fn [[m-child-field child-segment]]
                                                  [path-segment child-segment]))))))))]
    (if-not (= :relation (-> ctx :hypercrud.browser/field deref ::field/level)) ; omit relation-[]
      (conj (vec paths) [])                                 ; entity-[]
      (vec paths))))

(defn deps-satisfied? "Links in this :body strata" [ctx target-path]
  (let [this-path (:hypercrud.browser/path ctx)
        common-path (contrib.data/ancestry-common this-path target-path)
        common-ctx (context/refocus ctx common-path)]
    (loop [field @(:hypercrud.browser/field common-ctx)
           [x & xs] (contrib.data/ancestry-divergence target-path common-path)]
      (if-not x
        true                                                ; finished
        (let [child-field (context/find-child-field x field)]
          (case (::field/cardinality child-field)
            :db.cardinality/one (recur child-field xs)
            :db.cardinality/many false))))))

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

(defn ^:export select-all "List[Link]. Find the closest match."
  ; Not reactive! Track it outside. (r/track data/select-all ctx rel ?class)
  ([ctx]
   (->> @(:hypercrud.browser/links ctx)                     ; Reaction deref is why this belongs in a track
        (filter (comp (partial deps-satisfied? ctx)
                      link/read-path :link/path))))
  ([ctx rel] {:pre [rel]}
   (->> (select-all ctx)
        (filter #(= rel (:link/rel %)))))
  ([ctx rel ?class]
   (->> (select-all ctx rel)
        (filter (fn [link]
                  (if ?class
                    (boolean ((set (:link/class link)) ?class))
                    true))))))

(defn ^:export select+ "get a link for browsing later" [ctx rel & [?class]] ; Right[Reaction[Link]], Left[String]
  (let [link?s (r/track select-all ctx rel ?class)
        count @(r/fmap count link?s)]
    (cond
      (= 1 count) (right (r/fmap first link?s))
      (= 0 count) (left (str/format "no match for rel: %s class: %s" (pr-str rel) (pr-str ?class)))
      :else (left (str/format "Too many links matched for rel: %s class: %s" (pr-str rel) (pr-str ?class))))))

(defn ^:export browse+ "Navigate a link by hydrating its context accounting for dependencies in scope.
  returns Either[Loading|Error,ctx]."
  [ctx rel & [?class]]
  ; No focusing, can select from root, and data-from-link manufactures a new context
  (>>= (select+ ctx rel ?class)
       #(base/data-from-link @% ctx)))
