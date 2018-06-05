(ns hyperfiddle.ui
  (:require
    [clojure.walk :refer [prewalk]]
    [contrib.ui.remark :as remark]
    [contrib.reagent :refer [from-react-context]]
    [hypercrud.types.Entity :refer [Entity]]
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
