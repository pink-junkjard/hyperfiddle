(ns hyperfiddle.ui
  (:require
    [contrib.ui.remark :as remark]
    [contrib.reagent :refer [from-react-context]]
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
