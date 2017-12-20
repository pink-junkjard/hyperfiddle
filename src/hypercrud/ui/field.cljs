(ns hypercrud.ui.field
  (:require [cuerdas.core :as str]
            [hypercrud.browser.auto-anchor :as auto-anchor]
            [hypercrud.ui.connection-color :as connection-color]
            [hypercrud.ui.control.markdown-rendered :refer [markdown-rendered*]]
            [hypercrud.ui.css :refer [css-slugify]]
            [hypercrud.browser.anchor :as link]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.ui.tooltip :as tooltip]
            [hypercrud.util.core :as util]
            [hypercrud.util.reactive :as reactive]))


(defn attribute-human [attr]
  (->> (-> attr
           (util/update-existing :db/cardinality :db/ident)
           (util/update-existing :db/valueType :db/ident)
           (util/update-existing :db/unique :db/ident)
           (select-keys [:db/valueType :db/cardinality :db/unique]))
       (reduce-kv (fn [acc k v] (conj acc v)) [])))

(defn field-label [field ctx]
  (let [label (-> ctx :attribute :db/ident str)
        help-text (apply str (interpose " " (attribute-human (:attribute ctx))))]
    [tooltip/fast-hover-tooltip-managed
     {:label help-text
      :position :below-right}
     [:span.help label]]))

(defn field-block [control field links ctx]
  [:div {:class (str/join " " ["field" (-> ctx :attribute :db/ident str css-slugify)])
         :style {:border-color (connection-color/connection-color (:uri ctx) ctx)}}
   (let [[my-links] (as-> (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)]) $
                          (remove :link/dependent? $)       ; because we're in the label
                          (link/process-option-links $ ctx))]
     [:div.hc-label
      [:label [field-label field ctx]]
      [:div.anchors
       (link-controls/render-links (->> my-links (remove :link/render-inline?)) ctx)
       (link-controls/render-inline-links (->> my-links (filter :link/render-inline?)) ctx)]])
   (control ctx)
   [markdown-rendered* (-> ctx :attribute :db/doc) #() {:class "hypercrud-doc"}]])

(defn field-inline-block [control field links ctx]
  ; why are anchors unused
  (let [shadow-link (auto-anchor/system-anchor? (get-in ctx [:cell-data :db/id]))
        style {:border-color (if-not shadow-link (connection-color/connection-color (:uri ctx) ctx))}]
    [:td.truncate {:style style}
     [control ctx]]))

(defn Field [control field links ctx]
  (let [field-control (case (:layout ctx) :block field-block
                                          :inline-block field-inline-block
                                          :table field-inline-block)]
    [field-control control field links ctx]))

(defn ^:export with-field [Control]
  (fn [field links props ctx]
    (let [display-mode @(:display-mode ctx)
          ; What is the user-field allowed to change? The ctx. Can it change links or anchors? no.
          field-control (case display-mode :xray Field
                                           :user (get ctx :field Field))]
      [field-control (reactive/partial Control field links props) field links ctx])))
