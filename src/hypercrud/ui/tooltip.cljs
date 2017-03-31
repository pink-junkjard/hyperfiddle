(ns hypercrud.ui.tooltip
  (:require [reagent.core :as r]
            [re-com.core :as re-com]))


(defn hover-tooltip [& props]
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
                             :on-mouse-out #(do (if (:label t-props) (reset! state false)) nil)}
                      anchor]}))))))

(defn popover-anchor-wrapper* [& args]
  ; re-com has some weird "sugar" which inhibits dynamic props so turn it into a fn
  (apply vector re-com/popover-anchor-wrapper args))


(defn click-popover [& props]
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


(defn hover-popover [& props]
  (let [state (r/atom false)]
    (fn [t-props anchor]
      (apply
        popover-anchor-wrapper*
        (apply
          concat
          (merge
            {:position :below-center
             :showing? state}
            (dissoc t-props :body)
            {:anchor [:div {:on-mouse-enter #(do (if (:body t-props) (reset! state true)) nil)
                             :on-mouse-out #(do (if (:body t-props) (reset! state false)) nil)}
                      anchor]
             :popover [re-com/popover-content-wrapper
                       :on-cancel #(do (reset! state false) nil)
                       :no-clip? true
                       :body (:body t-props)]}))))))