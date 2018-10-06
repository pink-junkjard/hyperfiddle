(ns contrib.reagent
  (:require
    [contrib.reactive-debug :refer [track-cmp]]
    [goog.object]
    [prop-types :as prop-types]
    [reagent.core :as reagent]))


(defn ^:deprecated fragment [react-key & xs]
  (let [[k xs] (if (keyword? react-key)
                 [react-key xs]
                 [:_ (cons react-key xs)])]
    (into [:<> {:key (str k)}] xs)))

;(defn wrap-naked-string "wrap naked strings into divs" [C v]
;  (if (string? v) [:div v] v)
;  #_(let [hiccup-template (f value)]                          ; terrifying
;    (if (vector? hiccup-template)
;      hiccup-template
;      [C hiccup-template])))

(def with-react-context
  ; Reagent + react-context: https://github.com/reagent-project/reagent/commit/a8ec0d219bbd507f51a4d9276c4a1dcc020245af
  (reagent/create-class
    {:display-name "with-react-context"
     :reagent-render (fn [context template]
                       template)
     :child-context-types #js {"cljs-context" (.-object prop-types)}
     :get-child-context (fn []
                          (this-as this
                            (let [[_ context template] (reagent/argv this)]
                              #js {"cljs-context" context})))
     }))

(defn from-react-context "extract react context" [f]
  (reagent/create-class
    {:context-types #js {"cljs-context" (.-object prop-types)}
     :reagent-render (fn [& args]
                       (let [context (goog.object/get (.-context (reagent/current-component)) "cljs-context")]
                         (into [f context] args)))
     }))
