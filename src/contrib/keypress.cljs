(ns contrib.keypress
  (:require [reagent.core :as reagent]))


;(defprotocol Keypress
;  (simple-combo [this])
;  (simple-combo [this el])l
;  (register-combo [this el]))
;
;
;(def -keypress-instance (js/keypress.Listener.))
;
;(def ^:dynamic *keypress*
;  (reify Keypress
;
;    (simple-combo [this chord f!]
;      (.simple-combo -keypress-instance chord f!))
;
;    (simple-combo [this el chord f!]
;      (.simple-combo))
;    ))

(def with-keychord
  (reagent/create-class
    {:display-name "with-keychord"
     :reagent-render (fn [chord f! child]
                       child)
     :component-did-mount
     (fn [this]
       (let [[_ chord f! child] (reagent/argv this)
             el (reagent/dom-node this)
             listener (js/keypress.Listener. el)]
         #_(.register-combo listener #js {"keys" "esc"
                                          "on_keydown" f!
                                          "prevent_repeat" true})
         (.simple-combo listener chord f!)))
     }))
