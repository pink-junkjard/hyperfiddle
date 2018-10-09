(ns contrib.ui.remark
  (:require
    ["@hyperfiddle/remark-generic-extensions/lib/browser.min" :as remark-generic-extensions]
    [clojure.set]
    [clojure.string]
    [goog.object]
    [prop-types :as prop-types]
    [reagent.core :as reagent]
    [remark :as remark]
    [remark-react :as remark-react]))


(defn adapt-props [props]
  (-> props
      (dissoc :content :argument)                           ; need children downstack
      (clojure.set/rename-keys {:className :class})))

(defn extension [name f]
  (reagent/create-class
    {:display-name (str "markdown-" name)
     :context-types #js {:ctx (.-object prop-types)}
     :reagent-render (fn [{:keys [content argument] :as props}]
                       (let [ctx (goog.object/get (.-context (reagent/current-component)) "ctx")
                             content (if-not (= content "undefined") content)
                             argument (if-not (= argument "undefined") argument)]
                         (f content argument (adapt-props props) ctx)))}))

(defn -remark-render [remark-instance value]
  (let [c (-> remark-instance (.processSync value #js {"commonmark" true}) .-contents)
        content (-> c .-props .-children) #_"Throw away remark wrapper div"]
    content))

(defn -remark-instance! [extensions]
  (let [extensions (reduce-kv (fn [acc k v]
                                (assoc acc k (extension k v)))
                              (empty extensions)
                              extensions)]
    (-> (remark)
        (.use remark-generic-extensions
              (clj->js
                {"elements" (into {} (map vector (keys extensions) (repeat {"html" {"properties" {"content" "::content::" "argument" "::argument::"}}})))}))
        (.use remark-react (clj->js
                             {"sanitize" false
                              "remarkReactComponents" (reduce-kv (fn [m k v]
                                                                   (assoc m k (reagent/reactify-component v)))
                                                                 {} extensions)}))
        (cond->
          (exists? js/remarkComments) (.use js/remarkComments #js {"beginMarker" "" "endMarker" ""})
          (exists? js/remarkToc) (.use js/remarkToc)))))

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

       :child-context-types #js {:ctx (.-object prop-types)}})))
