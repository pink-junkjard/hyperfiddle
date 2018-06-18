(ns hyperfiddle.ui.form
  (:require
    [contrib.css :refer [css css-slugify]]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.ui.input :refer [keyword-input* edn-input*]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.system-link :refer [system-link?]]
    [hypercrud.ui.connection-color :refer [connection-color]]
    [hyperfiddle.ui.controls :as controls]
    [hyperfiddle.ui.sort :as sort]
    [cuerdas.core :as str]))


(defn border-color [ctx]
  (let [shadow-link @(r/fmap system-link? (r/fmap :db/id (context/entity ctx)))]
    (if-not shadow-link (connection-color ctx))))

(defn ui-block-border-wrap [ctx class & children]
  [:div {:class class :style {:border-color (border-color ctx)}}
   (apply fragment children)])

(defn magic-new-head [field ctx props]
  (let [entity (context/entity ctx)
        ;read-only (r/fmap (comp not controls/writable-entity?) entity) -- don't check this, '* always has a dbid and is writable
        state (r/cursor (::state ctx) [::magic-new-a])]
    ;(println (str/format "magic-new-head: %s , %s , %s" @state (pr-str @entity)))
    [keyword-input* @state (r/partial reset! state)
     (merge props {:placeholder ":task/title"})]))


(letfn [(change! [ctx entity state v]
          ((:user-with! ctx) [[:db/add @(r/fmap :db/id entity) @state v]]))]
  (defn magic-new-body [value ctx props]
    (let [entity (context/entity ctx)
          read-only (r/fmap (comp not controls/writable-entity?) entity)
          state (r/cursor (::state ctx) [::magic-new-a])]
      ;(println (str/format "magic-new-body: %s , %s , %s" @state @read-only (pr-str @entity)))
      ; Uncontrolled widget on purpose i think
      ; Cardinality many is not needed, because as soon as we assoc one value,
      ; we dispatch through a proper typed control
      [edn-input* nil (r/partial change! ctx entity state)
       (merge props {:read-only (let [_ [@state @read-only]] ; force reactions
                                  (or (nil? @state) @read-only))
                     :placeholder (pr-str "mow the lawn")})])))

(defn form-field "Form fields are label AND value. Table fields are label OR value."
  [hyper-control ?f ctx props]                              ; fiddle-src wants to fallback by passing nil here explicitly
  (assert @(:hypercrud.ui/display-mode ctx))
  (let [state (r/atom {::magic-new-a nil})]                 ; ^{:key (hash @(r/fmap keys (context/entity ctx)))}
    (fn [hyper-control ?f ctx props]
      (let [ctx (assoc ctx :hyperfiddle.ui.form/state state)]
        (ui-block-border-wrap
          ctx (css "field" (:class props))
          (let [head-ctx (dissoc ctx :relation)]
            [(or (:label-fn props) (hyper-control head-ctx)) (:hypercrud.browser/field head-ctx) head-ctx props])
          (if (:relation ctx)                               ; naked has no body
            [(or ?f (hyper-control ctx)) @(context/value ctx) ctx props]))))))

(defn table-field "Form fields are label AND value. Table fields are label OR value."
  [control-fac ?f ctx props]
  (let [{:keys [hypercrud.browser/field
                hypercrud.browser/attribute]} ctx
        [i a] [(:fe-pos ctx) attribute]
        path (remove nil? [i a])]
    (if (:relation ctx)
      [:td {:class (css "field" (:class props) "truncate")
            :style {:border-color (if i (border-color ctx))}}
       ; todo unsafe execution of user code: control
       [(or ?f (control-fac ctx)) @(context/value ctx) ctx props]]
      [:th {:class (css "field" (:class props)
                        (if (and i (sort/sortable? ctx path)) "sortable") ; hoist
                        (some-> (sort/sort-direction ctx) name)) ; hoist
            :style {:background-color (border-color ctx)}
            :on-click (r/partial sort/toggle-sort! ctx path)}
       ; Use f as the label control also, because there is hypermedia up there
       ((or (:label-fn props) (control-fac ctx)) field ctx props)])))

; (defmulti -field "Form fields are label AND value. Table fields are label OR value." ::layout)
(defn -field [ctx]
  (case (:hyperfiddle.ui/layout ctx)
    :hyperfiddle.ui.layout/table table-field
    form-field))
