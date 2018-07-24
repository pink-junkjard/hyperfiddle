(ns hyperfiddle.ui.form
  (:require
    [contrib.css :refer [css]]
    [contrib.reactive :as r]
    [hypercrud.browser.context :as context]
    [hypercrud.ui.connection-color :refer [border-color]]
    [hyperfiddle.ui.sort :as sort]))


(defn form-field "Form fields are label AND value. Table fields are label OR value."
  [hyper-control relative-path ctx ?f props]                ; ?f :: (ref props ctx) => DOM
  (let [state (r/atom {::magic-new-a nil})]
    (fn [hyper-control relative-path ctx ?f props]
      (let [ctx (assoc ctx ::state state)
            ; we want the wrapper div to have the :body styles, so careful not to pollute the head ctx with :body
            body-ctx (context/focus ctx (cons :body relative-path))
            props (update props :class css (hyperfiddle.ui/semantic-css body-ctx))]
        ; It is the driver-fn's job to elide this field if it will be empty
        [:div {:class (css "field" (:class props))
               :style {:border-color (border-color body-ctx)}}
         ^{:key :form-head}
         [(or (:label-fn props) hyper-control) props (context/focus ctx (cons :head relative-path))]
         ^{:key :form-body}
         [:div
          (if ?f
            [?f (:hypercrud.browser/data body-ctx) props body-ctx]
            [hyper-control props body-ctx])]]))))

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
