(ns hypercrud.ui.control.link-controls
  (:require [hypercrud.browser.core :as browser]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.control.link-controls-util :as link-controls-util]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.util.core :refer [kwargs]]))


(defn prompt [link-ref]
  (-> (or (:anchor/prompt @link-ref)                        ; hyperfiddle/hyperfiddle.net#124
          (:link/rel @link-ref)
          "_")
      str))

; ideally in the future we can infer path and dependent? from the ctx
(defn render-nav-cmps [path dependent? ctx & args]
  (let [{processors nil :as args} (kwargs args)]
    (->> (reactive/track link-controls-util/ui-contextual-links path dependent? false (:hypercrud.browser/links ctx) processors)
         (reactive/unsequence :db/id)
         (map (fn [[link-ref link-id]]
                (let [prompt @(reactive/track prompt link-ref)
                      browser ^{:key (hash link-id)} [(:navigate-cmp ctx) (link/build-link-props @link-ref ctx) prompt (:class args)]]
                  (if (not= :table (:layout ctx))
                    ^{:key (hash link-id)} [hypercrud.ui.form/ui-block-border-wrap ctx nil browser]
                    browser))))
         (doall))))

; ideally in the future we can infer path and dependent? from the ctx
(defn render-inline-links [path dependent? ctx & args]
  (let [{processors nil :as args} (kwargs args)]
    (->> (reactive/track link-controls-util/ui-contextual-links path dependent? true (:hypercrud.browser/links ctx) processors)
         (reactive/unsequence :db/id)
         (map (fn [[link-ref link-id]]
                ; don't test link validity, we need to render the failure. If this is a dependent link, use visibility predicate to hide the error.
                (let [prompt @(reactive/track prompt link-ref)
                      ; NOTE: this ctx logic and structure is the same as the inline branch of browser-request/recurse-request
                      browser ^{:key (hash link-id)} [browser/ui @link-ref ctx (:class args)]]
                  (if (not= :table (:layout ctx))
                    ^{:key (hash link-id)} [hypercrud.ui.form/ui-block-border-wrap ctx nil browser]
                    browser)
                  )))
         (doall))))
