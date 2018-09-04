(ns hypercrud.ui.connection-color
  (:require
    [contrib.reactive :as r]
    [cuerdas.core :as str]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.system-link :refer [system-link?]]))


; http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/

(def golden-ratio 0.618033988749895)
(def seed 0.3100632204946232 #_(.random js/Math))           ; i liked these colors

(def no-conn "#ccc")
(def root-conn "#777")

(defn hsl [h s l]
  (str/format "hsl(%s, %s%, %s%)" h s l))

(defn connection-color [ctx & [l]]
  (let [uri (context/uri ctx)]
    (condp = uri
      nil no-conn
      ;(get-in ctx [:hypercrud.browser/domain :domain/fiddle-database :database/uri]) root-conn
      (hsl (* 360 (mod (+ seed (* (hash uri) golden-ratio)) 1))
           55  #_"Too bright hurts the eyes"
           (or l 70)) #_"Medium gray (50) can be read on white and black backgrounds"
      )))

(letfn [(shadow-link? [ctx]
          (if (or (nil? ctx)
                  (nil? (:hypercrud.browser/data ctx)))
            false
            (if-let [dbid @(r/fmap :db/id (:hypercrud.browser/data ctx))]
              (system-link? dbid)
              (shadow-link? (:hypercrud.browser/parent ctx)))))]
  ; this entire function is a hack for the sys links editor
  ; we just want to call connection-color
  (defn border-color [ctx & [l]]
    (when-not (shadow-link? ctx)
      (connection-color ctx l))))
