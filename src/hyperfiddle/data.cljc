(ns hyperfiddle.data
  (:require
    [cats.core :refer [fmap >>=]]
    [cats.monad.either :refer [left right]]
    [contrib.data :refer [take-to]]
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

(defn- relative-links-at [path-segments ctx]                ; scary
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
        ; this result can be directly inserted as children in a reagemnt component, CANNOT be a vector
        seq)))

(defn deps-satisfied? "Links in this :body strata" [this-path link-path]
  ; TODO tighten - needs to understand WHICH find element is in scope
  (->> (contrib.data/ancestry-divergence link-path this-path)
       (drop-while (comp not #{:head :body}))
       empty?))

(defn deps-not-over-satisfied? [this-path link-path]
  ; Reject non-repeating links in a repeating context e.g. :xray mode
  (->> (contrib.data/ancestry-divergence this-path link-path)
       (drop-while (comp not #{:head :body}))
       empty?))

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
