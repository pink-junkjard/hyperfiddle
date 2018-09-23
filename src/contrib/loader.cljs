(ns contrib.loader
  (:require
    [cats.core :refer [mlet return]]
    [contrib.string :refer [blank->nil]]
    [goog.object]
    [promesa.core :as p]
    [reagent.core]))


(defn script! [href]
  (p/promise
    (fn [resolve reject]
      (let [el (js/document.createElement "script")]
        ; https://stackoverflow.com/questions/21214515/is-the-order-of-onload-handler-and-src-set-important-in-a-script-element
        ; https://stackoverflow.com/questions/16230886/trying-to-fire-the-onload-event-on-script-tag
        (doto el
          (goog.object/set "defer" true)                    ; In SSR case, defer onLoad until after body is parsed, but before DOMContentLoaded
          (goog.object/set "async" true)                    ; In non-SSR case, this reduces time-to-glass but can introduce a flash of "unstyled" content
          (goog.object/set "onload" #(resolve el))
          (goog.object/set "src" href)
          (js/document.body.appendChild))))))

(defn style! [href]
  (p/promise
    (fn [resolve reject]
      (let [el (js/document.createElement "link")]
        (doto el
          (goog.object/set "rel" "stylesheet")
          (goog.object/set "type" "text/css")
          (goog.object/set "href" href)
          (js/document.head.appendChild))
        (resolve el)))))


(defn load-asset! [href]
  (condp #(clojure.string/ends-with? %2 %1) href
    ".css" (style! href)
    ".js" (script! href)))

(def ^:export Loader!
  (reagent.core/create-class
    {:reagent-render (fn [])
     :component-did-mount
     (fn [this]
       (let [[_ asset-hrefs resolve! reject!] (reagent.core/argv this)]
         (-> (->> asset-hrefs
                  (filter blank->nil)
                  (map load-asset!)
                  doall)
             p/all
             (p/then (fn [els]
                       (goog.object/set this "refs" els)
                       (when resolve!
                         (resolve!)))))))
     :component-will-unmount
     (fn [this]
       (doseq [el (goog.object/get this "refs")]
         (.removeChild (.-parentNode el) el)))}))
