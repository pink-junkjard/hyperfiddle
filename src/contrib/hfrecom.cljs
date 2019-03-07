(ns contrib.hfrecom
  (:require
    [re-com.box]
    [re-com.core]
    [re-com.tabs]
    [re-com.util]
    [re-com.validate]))


(defn anchor-tabs "Modified from recom.tabs/horizontal-tabs to remove the ul classes and add href support"
  [& {:keys [model tabs on-change id-fn label-fn style]
      :or {id-fn :id label-fn :label}
      :as args}]
  {:pre [(re-com.validate/validate-args-macro re-com.tabs/tabs-args-desc args "tabs")]}
  (let [current (re-com.util/deref-or-value model)
        tabs (re-com.util/deref-or-value tabs)
        _ (assert (not-empty (filter #(= current (id-fn %)) tabs)) "model not found in tabs vector")]
    [:ul
     {:class "hfrecom-anchor-tabs noselect"                                     ; changed: was "rc-tabs nav nav-tabs noselect"
      :style (re-com.box/flex-child-style "none")}
     (for [t tabs]
       (let [id (id-fn t)
             label (label-fn t)
             selected? (= id current)]
         [:li
          {:class (if selected? "active")
           :key (str id)}
          [:a
           {:style (merge {:cursor "pointer"} style)
            :href (:href t)                                 ; added
            :on-click (when on-change (re-com.core/handler-fn (on-change id)))}
           label]]))]))
