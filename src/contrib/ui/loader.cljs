(ns contrib.ui.loader
  (:require
    [contrib.string :refer [blank->nil]]
    [goog.object]))


(defn script! [href callback]
  (doto (js/document.createElement "script")
    (goog.object/set "src" href)
    (goog.object/set "onload" callback)
    (js/document.body.appendChild)))

(defn style! [href]
  (doto (js/document.createElement "link")
    (goog.object/set "rel" "stylesheet")
    (goog.object/set "type" "text/css")
    (goog.object/set "href" href) (js/document.head.appendChild)))


(def ^:export Loader!
  (reagent.core/create-class
    {:reagent-render (fn [])
     :component-did-mount
     (fn [this]
       (let [[_ js-href css-href callback!] (reagent.core/argv this)]
         (->> [(if (blank->nil js-href) (script! js-href (or callback! (constantly nil))))
               (if (blank->nil css-href) (style! css-href))]
              (remove nil?)
              (goog.object/set this "refs"))))
     :component-will-unmount
     (fn [this]
       (doseq [el (goog.object/get this "refs")]
         (.removeChild (.-parentNode el) el)))}))
