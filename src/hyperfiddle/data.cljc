(ns hyperfiddle.data
  (:require
    [cats.core :refer [>>=]]
    [cats.monad.either :refer [left right]]
    [clojure.spec.alpha :as s]
    [contrib.ct :refer [unwrap]]
    [contrib.reactive :as r]
    [cuerdas.core :as str]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hyperfiddle.fiddle]))


(defn select-many "All links we can reach (for this entire dimension)"
  [ctx ?corcs]
  {:pre [#_(:hypercrud.browser/qfind ctx)                   ; :blank ?
         #_(:hypercrud.browser/element ctx)                 ; database color
         (:hypercrud.browser/link-index ctx)
         (s/assert :hypercrud/context ctx)
         #_(:hypercrud.browser/eav ctx)] ; it can be Reaction of [nil nil nil]
   :post [(r/reactive? %)]}                                 ; its also a vector, associative by index
  (->> (context/links-in-dimension-r
         ctx (contrib.data/xorxs ?corcs #{}))
       #_(map second)))

(defn validate-one+r [corcs r-links]                        ; Right[Reaction] or Left[Error] - broken, error should react too
  (let [n @(r/fmap count r-links)]
    (condp = n
      1 (right (r/fmap first r-links))
      0 (left (str/format "no match for class: %s" (pr-str corcs)))
      (left (str/format "Too many (%s) links matched for criteria: %s" n (pr-str corcs))))))

(defn ^:export select+ [ctx & [?corcs]]                     ; Right[Reaction[Link]] or Left[String]
  {:pre [(s/assert :hypercrud/context ctx)]}
  (->> (select-many ctx ?corcs)
       (validate-one+r ?corcs)
       #_#_r/apply-inner-r deref))

(defn ^:export select "reactive" [ctx & [?corcs]]
  {:pre [(s/assert :hypercrud/context ctx)]
   :post [(r/reactive? %)]}
  (->> (select+ ctx ?corcs)
       (unwrap (constantly (r/pure nil)))))

(defn select-many-here' [ctx & [?corcs]]
  (let [cs (contrib.data/xorxs ?corcs #{})
        a (context/a ctx)]
    ; links "here" means all possible links that apply. Can be lots of things!
    ; :db/id and :db/ident sys links
    ; :db/id and :db/ident parent attr links are good here too
    ; and attr tagged :db.unique/identity
    (concat
      @(select-many ctx (conj cs a))
      (if (and (context/identity? ctx a)                    ; Parent links apply here too!
               (not (context/qfind-level? ctx)))
        (let [ctx (context/unwind ctx 1)]
          @(select-many ctx (conj cs (context/a ctx))))))))

(defn ^:export select-many-here "reactive" ; collapses if eav is part of corcs
  [ctx & [?corcs]]
  {:pre [(s/assert :hypercrud/context ctx)]
   :post [(r/reactive? %)]}
  (r/track select-many-here' ctx ?corcs))

(defn ^:export select-here+ [ctx & [?corcs]]
  {:pre [(s/assert :hypercrud/context ctx)]
   :post [#_(r/reactive? %)]}                               ; Left[String]], Right[R[T]]
  (->> (select-many-here ctx ?corcs)
       (validate-one+r ?corcs)
       #_#_r/apply-inner-r deref))

(defn ^:export browse+ "Navigate a link by hydrating its context accounting for dependencies in scope.
  returns Either[Loading|Error,ctx]."
  [ctx & [?corcs]]
  {:pre [(s/assert :hypercrud/context ctx)]}
  ; No focusing, can select from root, and data-from-link manufactures a new context
  (>>= (select+ ctx ?corcs)                                 ; even one hflink can explode to two iframes
       #(base/data-from-link @% ctx)))                      ; can return tuple

(defn ^:export browse [ctx & [?corcs]]                      ; returns a ctx, not reactive, has reactive keys
  {:pre [(s/assert :hypercrud/context ctx)]}
  (->> (browse+ ctx ?corcs) (unwrap (constantly nil))))

(defn spread-links-here [ctx & [?corcs]]
  (->> (select-many-here ctx ?corcs)
       (r/unsequence :db/id)))

(defn spread-links-in-dimension [ctx & [?corcs]]            ; rename: layer
  (->> (select-many ctx ?corcs)
       (r/unsequence :db/id)))