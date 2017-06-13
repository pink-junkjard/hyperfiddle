(ns hypercrud.ui.markdown
  (:require [markdown.core :refer [md->html]]))


(defn markdown [value change! & [props]]
  [:div.markdown
   {:dangerouslySetInnerHTML {:__html (md->html value)}}])
