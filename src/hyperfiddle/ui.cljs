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
    [hypercrud.ui.auto-control :refer [auto-control]]
    [hypercrud.ui.form :as form]
    [hypercrud.ui.table :as table]
    [hyperfiddle.ui.markdown-extensions :refer [extensions]]))


(defn ^:export result [ctx & [?f]]
  (cond
    ?f [?f ctx]
    (:relations ctx) [table/table form/form ctx]
    (:relation ctx) [form/form ctx]))

(defn browse [rel path ctx ?f & args]
  (let [props (kwargs args)
        {:keys [:link/dependent? :link/path] :as link} @(r/track link/rel->link rel path ctx)
        ctx (-> (apply context/focus ctx dependent? (unwrap (memoized-safe-read-edn-string (str "[" path "]"))))
                (as-> ctx (if ?f (assoc ctx :user-renderer ?f #_(if ?f #(apply ?f %1 %2 %3 %4 args))) ctx)))]
    (into [browser/ui link ctx (:class props)] (apply concat (dissoc props :class :children nil)))))

(defn anchor [rel path ctx label & args]
  (let [{:keys [:link/dependent? :link/path] :as link} @(r/track link/rel->link rel path ctx)
        ctx (apply context/focus ctx dependent? (unwrap (memoized-safe-read-edn-string (str "[" path "]"))))
        props (kwargs args)]
    [(:navigate-cmp ctx) (merge props (link/build-link-props link ctx)) label (:class props)]))

(defn field [[i a] ctx ?f & args]
  (let [cell (case (::layout ctx) :hyperfiddle.ui.layout/table table/Field form/Field)]
    [(r/partial cell ?f)                                    ; Intentional explicit nil
     (context/focus ctx true i a)
     (kwargs args)]))

(defn value [[i a] ctx ?f & args]
  (let [ctx (context/focus ctx true i a)]
    [(or ?f (auto-control ctx)) @(:value ctx) ctx (kwargs args)]))

(defn browse' [rel path ctx]
  (->> (base/data-from-link @(r/track link/rel->link rel path ctx) ctx)
       (fmap :hypercrud.browser/result)
       (fmap deref)))

(defn ui-bindings [ctx]                                     ; legacy
  (assoc ctx
    :anchor anchor
    :browse browse
    :field field
    :cell field                                             ; legacy
    ::value value
    :browse' browse'))

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
