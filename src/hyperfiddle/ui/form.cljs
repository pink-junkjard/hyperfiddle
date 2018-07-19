(ns hyperfiddle.ui.form
  (:require
    [contrib.css :refer [css]]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.ui.input :refer [keyword-input* edn-input*]]
    [contrib.ui.tooltip :refer [tooltip-thick]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.system-link :refer [system-link?]]
    [hypercrud.ui.connection-color :refer [connection-color]]
    [hyperfiddle.ui.controls :as controls]
    [hyperfiddle.ui.docstring :refer [semantic-docstring]]
    [hyperfiddle.ui.sort :as sort]))


(letfn [(shadow-link? [ctx]
          (if (or (nil? ctx)
                  (nil? (:hypercrud.browser/data ctx))
                  (= :head (last (:hypercrud.browser/path ctx))))
            false
            (if-let [dbid @(r/fmap :db/id (:hypercrud.browser/data ctx))]
              (system-link? dbid)
              (shadow-link? (:hypercrud.browser/parent ctx)))))]
  (defn border-color [ctx]
    (when-not (shadow-link? ctx)                            ; this hack for sys links editor is quite the PITA
      (connection-color ctx))))

; todo pave me
(defn ui-block-border-wrap [ctx class & children]
  [:div {:class class :style {:border-color (border-color ctx)}}
   (apply fragment children)])

(defn magic-new-head [props ctx]
  (let [#_#_read-only (r/fmap (comp not controls/writable-entity?) (context/entity ctx)) ;-- don't check this, '* always has a dbid and is writable
        state (r/cursor (::state ctx) [::magic-new-a])]
    ;(println (str/format "magic-new-head: %s , %s , %s" @state (pr-str @entity)))
    [keyword-input* @state (r/partial reset! state)
     (merge props {:placeholder ":task/title"})]))


(letfn [(change! [ctx state v]
          ((:user-with! ctx) [[:db/add @(r/fmap :db/id (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])) @state v]]))]
  (defn magic-new-body [props ctx]
    (let [read-only (r/fmap (comp not controls/writable-entity?) (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))
          state (r/cursor (::state ctx) [::magic-new-a])]
      ;(println (str/format "magic-new-body: %s , %s , %s" @state @read-only (pr-str @entity)))
      ; Uncontrolled widget on purpose i think
      ; Cardinality many is not needed, because as soon as we assoc one value,
      ; we dispatch through a proper typed control
      [edn-input* nil (r/partial change! ctx state)
       (merge props {:read-only (let [_ [@state @read-only]] ; force reactions
                                  (or (nil? @state) @read-only))
                     :placeholder (pr-str "mow the lawn")})])))

(defn label [props ctx]
  (when-let [label (some->> (:hypercrud.browser/field ctx)
                            (r/fmap ::field/label)
                            deref)]
    (let [help-md (semantic-docstring ctx)]
      [tooltip-thick (if help-md
                       [:div.docstring [contrib.ui/markdown help-md]])
       [:label props label (if help-md [:sup "†"])]])))

(defn form-field "Form fields are label AND value. Table fields are label OR value."
  [hyper-control relative-path ctx ?f props]                ; ?f :: (ref props ctx) => DOM
  (assert @(:hypercrud.ui/display-mode ctx))
  (let [state (r/atom {::magic-new-a nil})]
    (fn [hyper-control relative-path ctx ?f props]
      (let [ctx (assoc ctx ::state state)
            ; we want the wrapper div to have the :body styles, so careful not to pollute the head ctx with :body
            body-ctx (context/focus ctx (cons :body relative-path))
            props (update props :class css (hyperfiddle.ui/semantic-css body-ctx))]
        ; It is the driver-fn's job to elide this field if it will be empty
        (ui-block-border-wrap
          body-ctx (css "field" (:class props))
          ^{:key :form-head}
          [(or (:label-fn props) hyper-control) props (context/focus ctx (cons :head relative-path))]
          ^{:key :form-body}
          [:div
           (if ?f
             [?f (:hypercrud.browser/data body-ctx) props body-ctx]
             [hyper-control props body-ctx])])))))

(defn table-field "Form fields are label AND value. Table fields are label OR value."
  [hyper-control relative-path ctx ?f props]                ; ?f :: (ref props ctx) => DOM
  (let [head-or-body (last (:hypercrud.browser/path ctx))   ; this is ugly and not uniform with form-field
        ctx (context/focus ctx relative-path)
        props (update props :class css (hyperfiddle.ui/semantic-css ctx))]
    (case head-or-body
      :head [:th {:class (css "field" (:class props)
                              (when (sort/sortable? ctx) "sortable") ; hoist
                              (some-> (sort/sort-direction relative-path ctx) name)) ; hoist
                  :style {:background-color (border-color ctx)}
                  :on-click (r/partial sort/toggle-sort! relative-path ctx)}
             ; Use f as the label control also, because there is hypermedia up there
             [(or (:label-fn props) hyper-control) props ctx]]
      :body [:td {:class (css "field" (:class props) "truncate")
                  :style {:border-color (when (:hypercrud.browser/source-symbol ctx) (border-color ctx))}}
             (if ?f
               [?f (:hypercrud.browser/data ctx) props ctx]
               [hyper-control props ctx])])))
