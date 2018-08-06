(ns hyperfiddle.ui.link-impl
  (:require
    [cats.monad.either :as either]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.string :refer [memoized-safe-read-edn-string]]
    [hypercrud.browser.core :as browser]
    [hypercrud.browser.link :as link]
    [hypercrud.ui.connection-color :refer [border-color]]))


(defn options-link? [link]
  ; don't care if its inline or not, just do the right thing.
  (= :options (:link/rel link)))

(def options-processor (partial remove options-link?))

(defn options-links [path links]
  (->> links
       (filter options-link?)
       (filter (partial link/draw-link? path))))

(defn draw-options? [path links]
  (not (empty? (options-links path links))))

(defn- prompt [link-ref]
  (str (or (some-> (:link/rel @link-ref) name) "_")))

; garbage wrapper for reactivity capturing
(defn- reactive-nav-cmp [link-ref ctx props]
  [(:navigate-cmp ctx) (merge (link/build-link-props @link-ref ctx) props) @(r/track prompt link-ref) (:class props)])

(defn- reactive-ui [link-ref ctx props]
  ; kwargs (dissoc props :class)
  [browser/ui @link-ref ctx (:class props)])

(defn contextual-links [path embed links ?processor]
  (->> (reduce (fn [links f] (f links)) @links (if ?processor [?processor]))
       ((if embed filter remove) (fn [link]
                                   (and (not (link/popover-link? link))
                                        (:link/render-inline? link))))
       ; path filtering is the most expensive, do it last
       (filter (partial link/draw-link? path))
       vec))

(defn anchors [path props ctx & [?processor]]
  (->> (r/track contextual-links path false (:hypercrud.browser/links ctx) ?processor)
       (r/unsequence :db/id)
       (map (fn [[link-ref link-id]]
              (if (not= :hyperfiddle.ui.layout/table (:hyperfiddle.ui/layout ctx))
                ^{:key (hash link-id)} [:div {:style {:border-color (border-color ctx)}}
                                        [reactive-nav-cmp link-ref ctx props]]
                ^{:key (hash link-id)} [reactive-nav-cmp link-ref ctx props])))
       (doall)
       (apply fragment)))

(defn iframes [path props ctx & [?processor]]
  (->> (r/track contextual-links path true (:hypercrud.browser/links ctx) ?processor)
       (r/unsequence :db/id)
       (map (fn [[link-ref link-id]]
              (if (not= :hyperfiddle.ui.layout/table (:hyperfiddle.ui/layout ctx))
                ^{:key (hash link-id)} [:div {:style {:border-color (border-color ctx)}}
                                        [reactive-ui link-ref ctx (:class props)]]
                ^{:key (hash link-id)} [reactive-ui link-ref ctx props])))
       (doall)
       (apply fragment)))
