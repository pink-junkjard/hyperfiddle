(ns hypercrud.ui.form
  (:require [cuerdas.core :as str]
            [hypercrud.browser.link :as link]
            [hypercrud.browser.context :as context]
            [hypercrud.react.react-fragment :refer [react-fragment]]
            [hypercrud.ui.connection-color :as connection-color]
            [hypercrud.ui.css :refer [css-slugify]]
            [hypercrud.ui.label :as label]
            [hypercrud.ui.auto-control :refer [auto-control' control-props]]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.ui.control.markdown-rendered :refer [markdown-rendered*]]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.ui.safe-render :refer [unify-portal-markup]]
            [taoensso.timbre :as timbre]))


(def ^:export Field nil)                                    ; compat

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

(defn form-label [field links ctx]
  (let [[my-links] (as-> (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)]) $
                         (remove :link/dependent? $)        ; because we're in the label
                         (link/process-option-links $ ctx))]
    [:div.hc-label
     [:label [label/label-inner field ctx]]
     [:div.anchors
      (link-controls/render-links (->> my-links (remove :link/render-inline?)) ctx)
      (link-controls/render-inline-links (->> my-links (filter :link/render-inline?)) ctx)]]))

(defn form-cell [control -field links ctx]
  [:div {:class (str/join " " ["field" (-> ctx :attribute :db/ident str css-slugify)])
         :style {:border-color (connection-color/connection-color (:uri ctx) ctx)}}
   [(:label ctx form-label) -field links ctx]
   [control -field links (control-props -field links ctx) ctx]
   #_[markdown-rendered* (-> ctx :attribute :db/doc) #() {:class "hypercrud-doc"}]])

(defn Cell [field links ctx]
  (let [ctx (as-> (context/attribute ctx (:attribute field)) $
                  (context/value $ ((:cell-data->value field) (:cell-data ctx)))
                  (if (or (nil? (:attribute field))
                          (= (:attribute field) :db/id))
                    (assoc $ :read-only always-read-only)
                    $))
        user-cell (case @(:display-mode ctx) :xray form-cell #_(unify-portal-markup form-cell)
                                             :user (:cell ctx form-cell))]
    (assert @(:display-mode ctx))
    ^{:key (:id field)}
    [user-cell (auto-control' ctx) field links ctx]))

(defn Entity [fe cell-data links ctx]
  (let [{inline-links true anchor-links false} (->> (link/links-lookup' links [(:fe-pos ctx)])
                                                    (remove :link/dependent?)
                                                    (group-by :link/render-inline?))]
    (concat
      (link-controls/render-links anchor-links ctx)
      (let [ctx (context/cell-data ctx cell-data)
            {inline-links true anchor-links false} (->> (link/links-lookup' links [(:fe-pos ctx)])
                                                        (filter :link/dependent?)
                                                        (group-by :link/render-inline?))]
        (concat
          (link-controls/render-links anchor-links ctx)
          (conj
            (->> (:fields fe)
                 (mapv (fn [field]
                         (Cell field links ctx))))
            (if (:splat? fe)
              ^{:key (hash (keys cell-data))}
              [new-field cell-data ctx]))
          (link-controls/render-inline-links inline-links ctx)))
      (link-controls/render-inline-links inline-links ctx))))

(defn Relation [relation ordered-fes links ctx]
  (let [ctx (assoc ctx :layout (:layout ctx :block))]
    [:div {:class (name (:layout ctx))}
     (->> ordered-fes
          (map-indexed (fn [fe-pos fe]
                         (let [ctx (context/find-element ctx fe fe-pos)]
                           (Entity fe (get relation fe-pos) links ctx))))
          (apply concat))]))
