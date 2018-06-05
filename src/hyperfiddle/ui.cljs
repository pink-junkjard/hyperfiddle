(ns hyperfiddle.ui
  (:require
    [clojure.walk :refer [prewalk]]
    [contrib.ui.remark :as remark]
    [contrib.reactive :as r]
    [contrib.reagent :refer [from-react-context fragment]]
    [hypercrud.browser.context :as context]
    [hypercrud.types.Entity :refer [Entity]]
    [hypercrud.ui.form]
    [hypercrud.ui.table]
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

;(defn result [ctx & [?f]]
;  (let [f (or ?f (if (:relations ctx)
;                   #(into [table] %&)
;                   form))]
;    (f form ctx)))
; This is not a reagent component; it returns a component-or-list-of-components (or nil).
; Thus it cannot be used from hiccup syntax. It needs to be wrapped into a :div or a react-fragment.
(defn result [ctx & [?f]]
  (cond
    ?f [?f ctx]
    (:relations ctx) [table form ctx]
    (:relation ctx) [form ctx]))
