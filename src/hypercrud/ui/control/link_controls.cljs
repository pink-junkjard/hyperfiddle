(ns hypercrud.ui.control.link-controls
  (:require [hypercrud.browser.core :as browser]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.control.link-controls-util :as link-controls-util]
            [hypercrud.util.reactive :as reactive]))


(defn prompt [link-ref]
  (-> (or (:anchor/prompt @link-ref)                        ; hyperfiddle/hyperfiddle.net#124
          (:link/rel @link-ref)
          "_")
      str))

; garbage wrapper for reactivity capturing
(defn- reactive-nav-cmp [link-ref ctx]
  [(:navigate-cmp ctx) (link/build-link-props @link-ref ctx) @(reactive/track prompt link-ref)])

; ideally in the future we can infer path and dependent? from the ctx
(defn render-nav-cmps [path dependent? ctx & processors]
  (let [links (reactive/track link-controls-util/ui-contextual-links path dependent? false (:hypercrud.browser/links ctx) processors)]
    (->> (reactive/unsequence :db/id links)
         (map (fn [[link-ref link-id]]
                ^{:key (hash link-id)}
                [reactive-nav-cmp link-ref ctx]))
         (doall))))

; garbage wrapper for reactivity capturing
(defn- reactive-ui [link-ref ctx]
  [browser/ui @link-ref ctx])

; ideally in the future we can infer path and dependent? from the ctx
(defn render-inline-links [path dependent? ctx & processors]
  (->> (reactive/track link-controls-util/ui-contextual-links path dependent? true (:hypercrud.browser/links ctx) processors)
       (reactive/unsequence :db/id)
       (map (fn [[link-ref link-id]]
              ; don't test link validity, we need to render the failure. If this is a dependent link, use visibility predicate to hide the error.
              [:div {:key (hash link-id)}                   ; extra div bc had trouble getting keys to work
               ; NOTE: this ctx logic and structure is the same as the inline branch of browser-request/recurse-request
               [reactive-ui link-ref ctx]]))
       (doall)))
