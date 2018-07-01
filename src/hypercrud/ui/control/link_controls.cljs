(ns hypercrud.ui.control.link-controls
  (:require
    [contrib.data :refer [kwargs]]
    [contrib.reactive :as r]
    [contrib.reagent :refer [from-react-context fragment]]
    [hyperfiddle.ui.form]
    ;[hypercrud.browser.core :as browser]
    [hypercrud.browser.link :as link]))


(defn prompt [link-ref]
  (str (or (some-> (:link/rel @link-ref) name) "_")))

; garbage wrapper for reactivity capturing
(defn- reactive-nav-cmp [link-ref ctx props]
  [(:navigate-cmp ctx) (merge (link/build-link-props @link-ref ctx) props) @(r/track prompt link-ref) (:class props)])

(defn- reactive-ui [link-ref ctx props]
  ; kwargs (dissoc props :class)
  [hypercrud.browser.core/ui @link-ref ctx (:class props)])

(defn ui-contextual-links [path embed links ?processor]
  (->> (reduce (fn [links f] (f links)) @links (if ?processor [?processor]))
       ((if embed filter remove) (fn [link]
                                   (and (not (link/popover-link? link))
                                        (:link/render-inline? link))))
       ;((case body-or-head :body filter :head remove) :link/dependent?)
       ; path filtering is the most expensive, do it last
       (filter (link/same-path-as? path))
       vec))

(def anchors
  (from-react-context
    (fn [{:keys [ctx props]} path & [?f]]   ; ?f should just be set of omitted rels
      (->> (r/track ui-contextual-links path false (:hypercrud.browser/links ctx) ?f)
           (r/unsequence :db/id)
           (map (fn [[link-ref link-id]]
                  (if (not= :hyperfiddle.ui.layout/table (:hyperfiddle.ui/layout ctx))
                    ^{:key (hash link-id)} [hyperfiddle.ui.form/ui-block-border-wrap ctx nil [reactive-nav-cmp link-ref ctx props]]
                    ^{:key (hash link-id)} [reactive-nav-cmp link-ref ctx props])))
           (doall)
           (apply fragment)))))

(def iframes
  (from-react-context
    (fn [{:keys [ctx props]} path & [?f]]
      (->> (r/track ui-contextual-links path true (:hypercrud.browser/links ctx) ?f)
           (r/unsequence :db/id)
           (map (fn [[link-ref link-id]]
                  (if (not= :hyperfiddle.ui.layout/table (:hyperfiddle.ui/layout ctx))
                    ^{:key (hash link-id)} [hyperfiddle.ui.form/ui-block-border-wrap ctx nil [reactive-ui link-ref ctx (:class props)]]
                    ^{:key (hash link-id)} [reactive-ui link-ref ctx props])))
           (doall)
           (apply fragment)))))
