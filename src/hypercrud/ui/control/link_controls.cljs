(ns hypercrud.ui.control.link-controls
  (:require [contrib.data :refer [kwargs]]
            [contrib.reactive :as r]
    ;[hypercrud.browser.core :as browser]
            [hypercrud.browser.link :as link]))


(defn prompt [link-ref]
  (str (or (some-> (:link/rel @link-ref) name) "_")))

; garbage wrapper for reactivity capturing
(defn- reactive-nav-cmp [link-ref ctx class]
  [(:navigate-cmp ctx) (link/build-link-props @link-ref ctx) @(r/track prompt link-ref) class])

(defn- reactive-ui [link-ref ctx class]
  [hypercrud.browser.core/ui @link-ref ctx class])

(defn ui-contextual-links [path dependent? inline? links ?processor]
  ; There is only one processor ?
  (->> (reduce (fn [links f] (f links)) @links (if ?processor [?processor]))
       ((if inline? filter remove) (fn [link]
                                     (and (not (link/popover-link? link))
                                          (:link/render-inline? link))))
       ((if dependent? filter remove) :link/dependent?)
       ; path filtering is the most expensive, do it last
       (filter (link/same-path-as? path))
       vec))

; ideally in the future we can infer path and dependent? from the ctx
; NOTE: this ctx logic and structure is the same as the inline branch of browser-request/recurse-request
; don't test link validity, we need to render the failure. If this is a dependent link, use visibility predicate to hide the error.

(defn anchors
  ; Can't infer dependent from :relation, must use :cell-data i think
  ([path dependent? ctx & [?f props]]
   (->> (r/track ui-contextual-links path dependent? false (:hypercrud.browser/links ctx) ?f)
        (r/unsequence :db/id)
        (map (fn [[link-ref link-id]]
               (if (not= :hyperfiddle.ui.layout/table (:hyperfiddle.ui/layout ctx))
                 ^{:key (hash link-id)} [hypercrud.ui.form/ui-block-border-wrap ctx nil [reactive-nav-cmp link-ref ctx (:class props)]]
                 ^{:key (hash link-id)} [reactive-nav-cmp link-ref ctx (:class props)])))
        (doall))))

(defn iframes
  ([path dependent? ctx & [?f props]]
   (->> (r/track ui-contextual-links path dependent? true (:hypercrud.browser/links ctx) ?f)
        (r/unsequence :db/id)
        (map (fn [[link-ref link-id]]
               (if (not= :hyperfiddle.ui.layout/table (:hyperfiddle.ui/layout ctx))
                 ^{:key (hash link-id)} [hypercrud.ui.form/ui-block-border-wrap ctx nil [reactive-ui link-ref ctx (:class props)]]
                 ^{:key (hash link-id)} [reactive-ui link-ref ctx (:class props)])))
        (doall))))
