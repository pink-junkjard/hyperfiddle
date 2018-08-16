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
  (-> (if (seq row)                                         ; todo should probably inspect fields instead of seq
        (map #(or (:db/id %) %) row)
        (or (:db/id row) row))
      hash))

(defn form "Field is invoked as fn"                         ; because it unifies with request fn side
  [ctx]                                                     ; f-field :: (relative-path ctx) => Any
  (-> (->> (r/fmap ::field/children (:hypercrud.browser/field ctx))
           (r/unsequence ::field/path-segment)
           (mapcat (fn [[field path-segment]]
                     (cond->> [[path-segment]]
                       @(r/fmap (r/comp not nil? ::field/source-symbol) field) ; this only happens once at the top for relation queries
                       (into (->> (r/fmap ::field/children field)
                                  (r/unsequence ::field/path-segment)
                                  (mapv (fn [[m-child-field child-segment]]
                                          [path-segment child-segment]))))))))
      vec
      (conj [])                                             ; row level (entity or relation etc)
      seq #_"no vectors in hiccup"))

(defn deps-satisfied? "Links in this :body strata" [this-path link-path]
  ; TODO tighten - needs to understand WHICH find element is in scope
  (let [left-divergence (contrib.data/ancestry-divergence link-path this-path)
        more-splits (->> left-divergence (filter #(= :body %)) count)]
    (= 0 more-splits)))

(defn link-path-floor [path]
  (->> path
       reverse
       (drop-while #(= :attribute (context/segment-type %)))
       reverse))

(defn deps-over-satisfied? "satisfied but not over-satisfied" [this-path link-path]
  (not= this-path (link-path-floor link-path)))

(defn ^:export select-all "List[Link]. Find the closest match. Can it search parent scopes for :options ?"
  ; Not reactive! Track it outside. (r/track data/select-all ctx rel ?class)
  ([ctx]
   (->> @(:hypercrud.browser/links ctx)                     ; Reaction deref is why this belongs in a track
        (filter (comp (partial deps-satisfied? (:hypercrud.browser/path ctx))
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
