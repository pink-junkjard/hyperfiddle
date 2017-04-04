(ns hypercrud.ui.tooltip
  (:require [reagent.core :as r]
            [re-com.core :as re-com]))


;(defn tooltip [& {:as props}]                               ; re-com style props
;  ; adapt standard props to re-com
;  (let [props (-> props
;                  (dissoc :body)                            ; leave :status and other props which don't need adaption
;                  (merge {:label (:body props)}))]
;    (apply re-com/popover-tooltip (apply concat props))))

(defn popover-anchor-wrapper* [& args]
  ; re-com has some weird "sugar" which inhibits dynamic props so turn it into a fn
  (apply vector re-com/popover-anchor-wrapper args))

;(defn popover [& {:as props}]
;  (let [props (-> props
;                  (dissoc :body)                            ; leave :status and other props which don't need adaption
;                  (merge {:popover
;                          [re-com/popover-content-wrapper
;                           :on-cancel #(do (reset! state false) nil)
;                           :no-clip? true
;                           :body (:body props)]
;
;                          }))]
;    (apply popover-anchor-wrapper* (apply concat props))))

(defn hover-tooltip [& args]
  ; if there is no tooltip, the hovers are such that it will never be shown
  (let [state (r/atom false)]
    (fn [t-props anchor]
      (apply
        re-com/popover-tooltip
        (apply
          concat
          (merge
            {:showing? state}
            t-props                                         ; label, status
            {:label (or (:label t-props) "")                ; required
             :anchor [:span {:on-mouse-enter #(do (if (:label t-props) (reset! state true)) nil)
                             :on-mouse-leave #(do (if (:label t-props) (reset! state false)) nil)}
                      anchor]}))))))

(defn click-popover [& args]
  (let [state (r/atom false)]
    (fn [t-props anchor]                                    ; magic :body key is documented at :popover :body
      (apply
        popover-anchor-wrapper*
        (apply
          concat
          (merge
            {:position :below-center
             :showing? state}
            (dissoc t-props :body)
            {:anchor [:span {:on-click #(do (swap! state not) nil)}
                      anchor]
             :popover [re-com/popover-content-wrapper
                       :on-cancel #(do (reset! state false) nil)
                       :no-clip? true
                       :body (:body t-props)]}))))))

(defn hover-popover* [& args]                               ; accept :label instead of :body to standardize
  (let [state (r/atom false)]
    (fn [t-props anchor]
      (apply
        popover-anchor-wrapper*
        (apply
          concat
          (merge
            {:position :below-center
             :showing? state}
            (dissoc t-props :label :status)                 ; just ignore status, todo fix
            {:anchor [:span {:on-mouse-enter #(do (if (:label t-props) (reset! state true)) nil)
                             :on-mouse-leave #(do (if (:label t-props) (reset! state false)) nil)}
                      anchor]
             :popover [re-com/popover-content-wrapper
                       :no-clip? true
                       :body (:label t-props)]}))))))

(defn hover-popover [& args]
  (apply vector hover-popover* args))
