(ns hypercrud.ui.control.link-controls
  (:require [hypercrud.browser.core :as browser]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.control.link-controls-util :as link-controls-util]))


(defn render-link [link ctx]
  (let [prompt (-> (or (:anchor/prompt link)                ; ???
                       (:link/rel link)
                       "_")
                   str)]
    [(:navigate-cmp ctx) (link/build-link-props link ctx) prompt]))

; ideally in the future we can infer path and dependent? from the ctx
(defn render-nav-cmps [path dependent? ctx & processors]
  ; what we want to write
  #_(let [links (reactive/track ui-contextual-links path dependent? false (:links ctx) processors)]
      (->> @(reactive/map (reactive/partial mapv :db/id) links)
           (map-indexed (fn [idx link-id]
                          ^{:key (hash link-id)}
                          [render-link @(reactive/map #(get % idx) links) ctx]))
           (doall)))

  (->> (link-controls-util/ui-contextual-links path dependent? false (:links ctx) processors)
       (map (fn [link]
              ^{:key (hash (:db/id link))}
              [render-link link ctx]))
       (doall)))

; ideally in the future we can infer path and dependent? from the ctx
(defn render-inline-links [path dependent? ctx & processors]
  (->> (link-controls-util/ui-contextual-links path dependent? true (:links ctx) processors)
       (map (fn [link]
              ; don't test link validity, we need to render the failure. If this is a dependent link, use visibility predicate to hide the error.
              [:div {:key (hash (:db/id link))}             ; extra div bc had trouble getting keys to work
               ; NOTE: this ctx logic and structure is the same as the inline branch of browser-request/recurse-request
               [browser/ui link (update ctx :hypercrud.browser/debug #(str % ">inline-link[" (:db/id link) ":" (or (:link/rel link) (:anchor/prompt link)) "]"))]]))
       (doall)))
