(ns hypercrud.ui.tooltip
  (:require [reagent.core :as r]
            [re-com.core :as re-com]))


(defn hover-tooltip [& props]
  (let [state (r/atom false)]
    (fn [t-props [anchor-cmp a-props & a-children :as anchor]] ; invasive destructure
      (apply
        re-com/popover-tooltip
        (apply
          concat
          (merge
            t-props                                         ; label, status
            {:label (or (:label t-props) "")                ; required
             :showing? state
             :anchor [:span {:on-mouse-over #(do (if (:label t-props) (reset! state true)) nil)
                             :on-mouse-out #(do (if (:label t-props) (reset! state false)) nil)} anchor]
             #_[anchor-cmp
                (merge a-props {:on-mouse-over #(do (if (:label t-props) (reset! state true)) nil)
                                :on-mouse-out #(do (if (:label t-props) (reset! state false)) nil)})
                a-children]}))))))