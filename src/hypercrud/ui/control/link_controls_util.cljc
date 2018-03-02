(ns hypercrud.ui.control.link-controls-util
  (:require [hypercrud.browser.link :as link]))


(defn ui-contextual-links [path dependent? inline? links processors]
  (->> (reduce (fn [links f] (f links)) @links processors)
       ((if inline? filter remove) (fn [link]
                                     (and (not (link/popover-link? link))
                                          (:link/render-inline? link))))
       ((if dependent? filter remove) :link/dependent?)
       ; path filtering is the most expensive, do it last
       (filter (link/same-path-as? path))
       vec))
