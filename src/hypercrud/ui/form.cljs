(ns hypercrud.ui.form
  (:require [cuerdas.core :as str]
            [hypercrud.browser.anchor :as link]
            [hypercrud.browser.context :as context]
            [hypercrud.ui.connection-color :as connection-color]
            [hypercrud.ui.field :as field]
            [hypercrud.ui.attribute :refer [Attribute]]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.user-attribute-renderer :as renderer]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.util.reactive :as reactive]))


(def ^:export with-field field/with-field)                  ; compat

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
                     (let [ctx (as-> (context/attribute ctx (:attribute field)) $
                                     (context/value $ ((:cell-data->value field) (:cell-data ctx)))
                                     (if (or (nil? (:attribute field))
                                             (= (:attribute field) :db/id))
                                       (assoc $ :read-only always-read-only)
                                       $))]
                       (if (renderer/user-attribute-renderer ctx)
                         (renderer/user-attribute-render field links {} ctx)
                         ^{:key (:id field)}
                         [Attribute field links {} ctx])
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
