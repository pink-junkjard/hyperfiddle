(ns hypercrud.ui.markdown)

(def showdown (delay (js/showdown.Converter.)))

(defn markdown [value change! & [props]]
  [:div.markdown
   {:class (:class props)
    :dangerouslySetInnerHTML {:__html (.makeHtml @showdown value)}}])
