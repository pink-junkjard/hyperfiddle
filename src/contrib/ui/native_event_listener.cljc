(ns contrib.ui.native-event-listener
  (:require
    #?(:cljs [reagent.core :as reagent])
    #?(:cljs [reagent.impl.util :refer [PartialFn]])))


#?(:cljs
   ; addEventListener chokes HARD on reagent/partialed functions,
   ; it wants a javascript function or an object:
   ; see: "listener" param at https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener
   ; when adapting props for adapated react classes, reagent wraps ALL IFn in an anonymous fn
   ; like: (fn [&args] (apply user-fn args))
   ; see reagent.impl.template/convert-prop-value
   ; since reagent isnt adapting the props for create-class, we have to do it ourselves
   ; and we cannot use reagent's because we need equality on the fn for removeEventListener
   (defn adapt-fn [x] (if (instance? PartialFn x) (.-pfn x) x)))

(def native-on-click-listener
  #?(:clj  (fn [& args] (assert false "todo"))
     :cljs (reagent/create-class
             {:display-name "native-on-click-listener"
              :reagent-render (fn [_ child] child)
              :component-did-mount (fn [this]
                                     (let [[_ props _] (reagent/argv this)
                                           el (reagent/dom-node this)]
                                       (when-let [on-click (some-> (:on-click props) adapt-fn)]
                                         (.addEventListener el "click" on-click false))))
              :component-did-update (fn [this [_ prev-props _]]
                                      (let [[_ props _] (reagent/argv this)
                                            el (reagent/dom-node this)]
                                        (when-let [on-click (some-> (:on-click prev-props) adapt-fn)]
                                          (.removeEventListener el "click" on-click false))
                                        (when-let [on-click (some-> (:on-click props) adapt-fn)]
                                          (.addEventListener el "click" on-click false))))
              })))
