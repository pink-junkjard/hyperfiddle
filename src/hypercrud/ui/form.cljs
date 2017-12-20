(ns hypercrud.ui.form
  (:require [cuerdas.core :as str]
            [hypercrud.browser.anchor :as link]
            [hypercrud.browser.context :as context]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.connection-color :as connection-color]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.control.markdown-rendered :refer [markdown-rendered*]]
            [hypercrud.ui.user-attribute-renderer :as renderer]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.util.reactive :as reactive]))


(defn Field [control field links ctx]
  [:div {:class (str/join " " ["field" (-> ctx :attribute :db/ident str form-util/css-slugify)])
         :style {:border-color (connection-color/connection-color (:uri ctx) ctx)}}
   (let [[my-links] (as-> (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)]) $
                          (remove :link/dependent? $)       ; because we're in the label
                          (link/process-option-links $ ctx))]
     [:div.hc-label
      [:label [form-util/field-label field ctx]]
      [:div.anchors
       (link-controls/render-links (->> my-links (remove :link/render-inline?)) ctx)
       (link-controls/render-inline-links (->> my-links (filter :link/render-inline?)) ctx)]])
   (control ctx)
   [markdown-rendered* (-> ctx :attribute :db/doc) #() {:class "hypercrud-doc"}]])

(defn new-field [entity ctx]
  (let [attr-ident (reactive/atom nil)]
    (fn [entity ctx]
      [:div.field {:style {:border-color (connection-color/connection-color (:uri ctx) ctx)}}
       [:div.hc-label
        [:label
         (let [on-change! #(reset! attr-ident %)]
           [input/keyword-input* @attr-ident on-change!])]]
       (let [on-change! #(let [tx [[:db/add (:db/id entity) @attr-ident %]]]
                           ; todo cardinality many
                           ((:user-with! ctx) tx))
             props (if (nil? @attr-ident) {:read-only true})]
         [input/edn-input* nil on-change! props])])))

(def always-read-only (constantly true))

; attribute renderers works in :xray mode, like select options
; ctx overrides are :user mode

(defn Control [field links props ctx]
  [auto-control field links props ctx])

(defn with-field [Control]
  (fn [field links props ctx]
    [Field (reactive/partial Control field links props) field links ctx]))

(defn Attribute [field links props ctx]
  (let [display-mode @(:display-mode ctx)
        ; What is the user-field allowed to change? The ctx. Can it change links or anchors? no.
        Field (case display-mode :xray Field :user (get ctx :field Field))
        Control (case display-mode :xray Control :user (get ctx :control Control))]
    ; todo control can have access to repeating contextual values (color, owner, result, entity, value, etc) but field should NOT
    ; this leads to inconsistent location formulas between non-repeating links in tables vs forms
    [(with-field Control) field links props ctx]))

(defn cell-data-fields [fe cell-data links ctx]
  (let [ctx (context/cell-data ctx cell-data)
        {inline-links true anchor-links false} (->> (link/links-lookup' links [(:fe-pos ctx)])
                                                    (filter :link/dependent?)
                                                    (group-by :link/render-inline?))]
    (concat
      (link-controls/render-links anchor-links ctx)
      (conj
        (->> (:fields fe)
             (mapv (fn [field]
                     ^{:key (:id field)}
                     (let [ctx (as-> (context/attribute ctx (:attribute field)) $
                                     (context/value $ ((:cell-data->value field) (:cell-data ctx)))
                                     (if (or (nil? (:attribute field))
                                             (= (:attribute field) :db/id))
                                       (assoc $ :read-only always-read-only)
                                       $))
                           props (form-util/build-props field links ctx)]
                       (if (renderer/user-attribute-renderer ctx)
                         (renderer/user-attribute-render field links props ctx)
                         [Attribute field links props ctx])
                       ))))
        (if (:splat? fe)
          ^{:key (hash (keys cell-data))}
          [new-field cell-data ctx]))
      (link-controls/render-inline-links inline-links ctx))))

(defn result-cell [fe cell-data links ctx]
  (let [{inline-links true anchor-links false} (->> (link/links-lookup' links [(:fe-pos ctx)])
                                                    (remove :link/dependent?)
                                                    (group-by :link/render-inline?))]
    (concat
      (link-controls/render-links anchor-links ctx)
      (cell-data-fields fe cell-data links ctx)
      (link-controls/render-inline-links inline-links ctx))))

(defn Relation [relation ordered-fes links ctx]
  (let [ctx (assoc ctx :layout (:layout ctx :block))]
    [:div {:class (name (:layout ctx))}
     (->> ordered-fes
          (map-indexed (fn [fe-pos fe]
                         (let [ctx (context/find-element ctx fe fe-pos)]
                           (result-cell fe (get relation fe-pos) links ctx))))
          (apply concat))]))
