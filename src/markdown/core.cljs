(ns markdown.core)

(def showdown (delay (js/showdown.Converter.)))

(defn md->html [value]
  (.makeHtml @showdown value))
