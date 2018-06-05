(ns hyperfiddle.ui
  (:require
    [cats.core :refer [mlet fmap]]
    [clojure.walk :refer [prewalk]]
    [contrib.data :refer [unwrap kwargs]]
    [contrib.string :refer [memoized-safe-read-edn-string]]
    [contrib.ui.remark :as remark]
    [contrib.reactive :as r]
    [contrib.reagent :refer [from-react-context fragment]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.core :as browser]
    [hypercrud.browser.link :as link]
    [hypercrud.types.Entity :refer [Entity]]
    [hypercrud.ui.auto-control :as auto-control]
    [hypercrud.ui.form :as form-]
    [hypercrud.ui.table :as table-]
    [hyperfiddle.ui.markdown-extensions :refer [extensions]]))


(def -remark-instance (remark/remark
                        (reduce-kv (fn [acc k v]
                                     (assoc acc k (remark/extension k v)))
                                   (empty extensions)
                                   extensions)))

(defn ^:export markdown [& args]
  (into [remark/markdown -remark-instance] args))

(def ^:export img
  (from-react-context
    (fn [{:keys [ctx props]} value]
      [:img (merge props {:src value})])))

; [contrib.ui/code-block {} (contrib.pprint/pprint-str (hyperfiddle.ui/pull-soup->tree @fiddle) 40)]
(defn ^:export pull-soup->tree [pull]
  (prewalk (fn [v]
             (if (instance? Entity v) (into {} v) v))
           pull))

(defn form [{:keys [hypercrud.browser/ordered-fes
                    hypercrud.browser/links] :as ctx}]
  (let [ctx (assoc ctx :hyperfiddle.ui/layout (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block))]
    (->> (r/unsequence ordered-fes)
         ; Wouldn't mlet be nice here
         (mapcat (fn [[fe i]]
                   (->> fe
                        (r/fmap :fields)
                        (r/unsequence :attribute)
                        (map (fn [[_ a]]
                               ^{:key (str i a)}
                               [(:field ctx) [i a] ctx])))))
         (apply fragment :_))))

(defn table [form ctx]
  (let [sort-col (r/atom nil)]
    (fn [form ctx]
      (let [ctx (assoc ctx :hyperfiddle.ui/layout (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/table)
                           :hypercrud.ui.table/sort-col sort-col
                           ::unp true)]
        [:table.ui-table.unp
         [:thead [form ctx]]
         [:tbody (->> (:relations ctx)
                      (r/unsequence hypercrud.ui.table/relation-keyfn)
                      (map (fn [[relation k]]
                             ^{:key k}
                             [:tr [form (context/relation ctx relation)]]))
                      (doall))]]))))

(defn result [ctx & [?f]]
  (cond
    ?f [?f ctx]
    (:relations ctx) [table form ctx]
    (:relation ctx) [form ctx]))

(letfn [(browse [rel #_dependent? path ctx ?f & args]
          (let [props (kwargs args)
                {:keys [:link/dependent? :link/path] :as link} @(r/track link/rel->link rel path ctx)
                ctx (-> (context/relation-path ctx (into [dependent?] (unwrap (memoized-safe-read-edn-string (str "[" path "]")))))
                        (as-> ctx (if ?f (assoc ctx :user-renderer ?f #_(if ?f #(apply ?f %1 %2 %3 %4 args))) ctx)))]
            (into [browser/ui link ctx (:class props)] (apply concat (dissoc props :class :children nil)))))
        (anchor [rel #_dependent? path ctx label & args]
          (let [{:keys [:link/dependent? :link/path] :as link} @(r/track link/rel->link rel path ctx)
                ctx (context/relation-path ctx (into [dependent?] (unwrap (memoized-safe-read-edn-string (str "[" path "]")))))
                props (kwargs args)]
            [(:navigate-cmp ctx) (merge props (link/build-link-props link ctx)) label (:class props)]))
        (field [[i a] ctx ?f & args]
          (let [cell (case (::layout ctx) :hyperfiddle.ui.layout/table table-/Field form-/Field)]
            [(r/partial cell ?f)                            ; Intentional explicit nil
             (context/relation-path ctx [true i a])
             (kwargs args)]))
        (value [[i a] ctx ?f & args]
          (let [ctx (context/relation-path ctx [true i a])]
            [(or ?f (auto-control/auto-control ctx)) @(:value ctx) ctx (kwargs args)]))
        (browse' [rel #_dependent? path ctx]
          (->> (base/data-from-link @(r/track link/rel->link rel path ctx) ctx)
               (fmap :hypercrud.browser/result)
               (fmap deref)))]
  ; convenience functions, should be declared fns in this or another ns and accessed out of band of ctx
  (defn ui-bindings [ctx]
    (assoc ctx
      :anchor anchor
      :browse browse
      :field field
      :cell field                                           ; legacy
      ::value value
      :browse' browse')))
