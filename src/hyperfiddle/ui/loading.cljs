(ns hyperfiddle.ui.loading)


(defn loading-page [& children]
  [:div.loading-page>div.logo-callout
   [:img {:src "https://i.imgur.com/DtMAeuM.png"}]
   [:span.brand ":hyperfiddle"]
   (when children
     (into [:div.additional-content] children))])
