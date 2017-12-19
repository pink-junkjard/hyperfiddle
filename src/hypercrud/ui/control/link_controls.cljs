(ns hypercrud.ui.control.link-controls
  (:require [hypercrud.browser.core :as browser]
            [hypercrud.browser.anchor :as link]))


(defn render-link [link ctx]
  (let [prompt (or (:anchor/prompt link)                    ; ???
                   (:link/rel link)
                   "_")]
    [(:navigate-cmp ctx) (link/build-link-props link ctx) prompt]))

(defn render-links
  ([link-ctx-pairs]
   (->> link-ctx-pairs
        ; Don't filter hidden links; because they could be broken or invalid and need to draw error.
        (map (fn [[link ctx]]
               ^{:key (hash (:db/id link))}
               [render-link link ctx]))
        doall))
  ([links ctx]
   (render-links (map vector links (repeat ctx)))))

(defn render-inline-links
  ([links ctx]
   (render-inline-links (map vector links (repeat ctx))))
  ([link-ctx-pairs]
   (->> link-ctx-pairs
        ; Don't filter hidden links; because they could be broken or invalid and need to draw error.
        (map (fn [[link ctx]]
               ; don't test link validity, we need to render the failure. If this is a dependent link, use visibility predicate to hide the error.
               [:div {:key (hash (:db/id link))}            ; extra div bc had trouble getting keys to work
                ; NOTE: this ctx logic and structure is the same as the inline branch of browser-request/recurse-request
                [browser/ui link (update ctx :debug #(str % ">inline-link[" (:db/id link) ":" (or (:link/rel link) (:anchor/prompt link)) "]"))]]))
        (remove nil?)
        (doall))))
