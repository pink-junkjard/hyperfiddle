(ns hyperfiddle.ui.form
  (:require
    [contrib.css :refer [css]]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment from-react-context with-react-context]]
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
                  (#{:head :body} (last (:hypercrud.browser/path ctx))))
            false
            (if-let [dbid @(r/fmap :db/id (:hypercrud.browser/data ctx))]
              (system-link? dbid)
              (shadow-link? (:hypercrud.browser/parent ctx)))))]
  (defn border-color [ctx]
    (when-not (shadow-link? ctx)                            ; this hack for sys links editor is quite the PITA
      (connection-color ctx))))

(defn ui-block-border-wrap [ctx class & children]
  [:div {:class class :style {:border-color (border-color ctx)}}
   (apply fragment children)])

(def magic-new-head
  (from-react-context
    (fn [{:keys [ctx props]} field]
      (let [#_#_read-only (r/fmap (comp not controls/writable-entity?) (context/entity ctx)) ;-- don't check this, '* always has a dbid and is writable
            state (r/cursor (::state ctx) [::magic-new-a])]
        ;(println (str/format "magic-new-head: %s , %s , %s" @state (pr-str @entity)))
        [keyword-input* @state (r/partial reset! state)
         (merge props {:placeholder ":task/title"})]))))


(letfn [(change! [ctx state v]
          ((:user-with! ctx) [[:db/add @(r/fmap :db/id (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])) @state v]]))]
  (def magic-new-body
    (from-react-context
      (fn [{:keys [ctx props]} value]
        (let [read-only (r/fmap (comp not controls/writable-entity?) (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))
              state (r/cursor (::state ctx) [::magic-new-a])]
          ;(println (str/format "magic-new-body: %s , %s , %s" @state @read-only (pr-str @entity)))
          ; Uncontrolled widget on purpose i think
          ; Cardinality many is not needed, because as soon as we assoc one value,
          ; we dispatch through a proper typed control
          [edn-input* nil (r/partial change! ctx state)
           (merge props {:read-only (let [_ [@state @read-only]] ; force reactions
                                      (or (nil? @state) @read-only))
                         :placeholder (pr-str "mow the lawn")})])))))

(def label
  (from-react-context
    (fn [{:keys [ctx props]} field]
      (let [help-md (semantic-docstring ctx)]
        [tooltip-thick (if help-md
                         [:div.docstring [contrib.ui/markdown help-md]])
         [:label props (::field/label field) (if help-md [:sup "†"])]]))))

(defn form-field "Form fields are label AND value. Table fields are label OR value."
  [hyper-control relative-path ctx ?f props]                ; fiddle-src wants to fallback by passing nil here explicitly
  (assert @(:hypercrud.ui/display-mode ctx))
  (let [state (r/atom {::magic-new-a nil})]
    (fn [hyper-control relative-path ctx ?f props]
      (let [ctx (assoc ctx :hyperfiddle.ui.form/state state)
            ; we want the wrapper div to have the :body styles, so build the head before polluting the ctx with :body
            head (let [ctx (context/focus ctx (cons :head relative-path))
                       field (some-> (:hypercrud.browser/field ctx) deref)] ; todo unbreak reactivity
                   ^{:key :form-head}
                   [with-react-context {:ctx ctx :props props}
                    [(or (:label-fn props) (hyper-control ctx)) field]])
            ctx (context/focus ctx (cons :body relative-path))
            props (update props :class css (hyperfiddle.ui/semantic-css ctx))]
        (ui-block-border-wrap
          ctx (css "field" (:class props))
          head
          (let [data @(:hypercrud.browser/data ctx)]        ; todo why consciously break reactivity
            #_(if (:relation ctx))                          ; naked has no body
            ^{:key :form-body}
            [with-react-context {:ctx ctx :props props}
             [(or ?f (hyper-control ctx)) data]]))))))

(defn table-field "Form fields are label AND value. Table fields are label OR value."
  [control-fac relative-path ctx ?f props]
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
             (let [field (some-> (:hypercrud.browser/field ctx) deref)] ; todo unbreak reactivity
               [with-react-context {:ctx ctx :props props}
                [(or (:label-fn props) (control-fac ctx)) field]])]
      :body (let [data @(:hypercrud.browser/data ctx)]      ; todo why consciously break reactivity
              [:td {:class (css "field" (:class props) "truncate")
                    :style {:border-color (when (:hypercrud.browser/source-symbol ctx) (border-color ctx))}}
               ; todo unsafe execution of user code: control
               [with-react-context {:ctx ctx :props props}
                [(or ?f (control-fac ctx)) data]]]))))
