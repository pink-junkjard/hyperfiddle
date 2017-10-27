(ns markdown.core
  (:require [reagent.core :as r]))

(def showdown (delay (js/showdown.Converter.)))

(defn md->html [value]
  (.makeHtml @showdown value))
