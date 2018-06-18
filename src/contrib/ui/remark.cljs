(ns contrib.ui.remark
  (:require
    [clojure.string]
    [goog.object]
    [reagent.core :as reagent]))


(defn extension [name f]
  (reagent/create-class
    {:display-name (str "markdown-" name)
     :context-types #js {:ctx js/propTypes.object}
     :reagent-render (fn [{:keys [content argument] :as props}]
                       (let [ctx (goog.object/get (.-context (reagent/current-component)) "ctx")
                             content (if-not (= content "undefined") content)
                             argument (if-not (= argument "undefined") argument)]
                         (f content argument (dissoc props :content :argument) ctx)))}))

(defn -remark-render [remark-instance value]
  (let [c (-> remark-instance (.processSync value #js {"commonmark" true}) .-contents)
        content (-> c .-props .-children) #_"Throw away remark wrapper div"]
    content))

(defn -remark-instance! [extensions]
  (let [extensions (reduce-kv (fn [acc k v]
                                (assoc acc k (extension k v)))
                              (empty extensions)
                              extensions)]
    (-> (js/remark)
        (.use js/remarkGenericExtensions
              (clj->js
                {"elements" (into {} (map vector (keys extensions) (repeat {"html" {"properties" {"content" "::content::" "argument" "::argument::"}}})))}))
        (.use js/remarkReact
              (clj->js
                {"sanitize" false
                 "remarkReactComponents" (reduce-kv (fn [m k v]
                                                      (assoc m k (reagent/reactify-component v)))
                                                    {} extensions)})))))

(defn remark! [& [extensions]]
  ; remark creates react components which don't evaluate in this stack frame
  ; so dynamic scope is not helpful to communicate values to remark plugins
  ; Reagent + react-context: https://github.com/reagent-project/reagent/commit/a8ec0d219bbd507f51a4d9276c4a1dcc020245af
  (let [remark-instance (-remark-instance! extensions)]
    (reagent/create-class
      {:display-name "markdown"
       :reagent-render
       (fn [value & [?ctx]]
         (when-not (clojure.string/blank? value)
           (-remark-render remark-instance value)))

       :get-child-context
       (fn []
         (this-as this
           (let [[_ value ?ctx] (reagent/argv this)]
             #js {:ctx ?ctx})))

       :child-context-types #js {:ctx js/propTypes.object}})))
