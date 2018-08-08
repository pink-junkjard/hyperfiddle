(ns hyperfiddle.ui.select
  (:require
    [cats.context :refer-macros [with-context]]
    [cats.core :as cats :refer [fmap]]
    [cats.monad.either :as either]
    [contrib.reactive :as r]
    [contrib.try$ :refer [try-either]]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.link :as link]
    [hypercrud.ui.error :as ui-error]
    [hypercrud.types.Entity :refer [entity?]]
    [hyperfiddle.data :as data]
    [hyperfiddle.ui.controls :as controls]))


(defn default-label-renderer [v ctx]
  (if (instance? cljs.core/Keyword v)
    (name v)                                                ; A sensible default for userland whose idents usually share a long namespace.
    (str v))
  #_(condp (fn [x c] (instance? c x)) v
      cljs.core/Keyword (name v)
      (str v)))

(defn label-fn [data ctx]                                   ; It's perfectly possible to properly report this error properly upstream. (later - is it?)
  ; This whole thing is legacy, migrate it to link props
  (let [elabels (->> (map (fn [data field]
                            (->> (::field/children field)
                                 (mapv (fn [{:keys [::field/get-value ::field/path-segment]}]
                                         ; Custom label renderers? Can't use the attribute renderer, since that
                                         ; is how we are in a select options in the first place.
                                         (let [renderer (get-in ctx [:fields path-segment :label-renderer] default-label-renderer)]
                                           (try-either (renderer (get-value data) ctx)))))))
                          data
                          @(:hypercrud.browser/fields ctx))
                     (apply concat))
        label' (->> (with-context either/context (cats/sequence elabels)) ; prevents cats no context set errors
                    (fmap #(->> % (interpose ", ") (remove nil?) (apply str))))]
    (either/branch label' pr-str identity)))

(defn select-anchor-renderer' [props option-props ctx]
  ; hack in the selected value if we don't have options hydrated?
  ; Can't, since we only have the #DbId hydrated, and it gets complicated with relaton vs entity etc
  (let [no-options? @(r/fmap empty? (:hypercrud.browser/data ctx))
        props (-> props
                  (update :on-change (fn [on-change]
                                       (fn [e]
                                         (let [select-value (.-target.value e)
                                               id (when (not= "" select-value)
                                                    (let [id (js/parseInt select-value 10)]
                                                      (if (< id 0) (str id) id)))]
                                           (on-change id)))))
                  ; Don't disable :select if there are options, we may want to see them. Make it look :disabled but allow the click
                  (update :disabled #(or % no-options?))
                  (update :class #(str % (if (:disabled option-props) " disabled"))))
        label-fn (:label-fn props label-fn)
        id-fn (fn [relation]
                (let [fe (first relation)]
                  (if (or (entity? fe) (map? fe))           ; todo inspect datalog find-element
                    (:db/id fe)
                    fe)))]
    [:select.ui (dissoc props :label-fn)
     ; .ui is because options are an iframe and need the pink box
     (conj
       (->> @(:hypercrud.browser/data ctx)
            (mapv (juxt id-fn #(label-fn % ctx)))
            (sort-by second)
            (map (fn [[id label]]
                   [:option (assoc option-props :key (str id) :value id) label])))
       [:option (assoc option-props :key :blank :value "") "--"])]))

(defn select-error-cmp [msg]
  [:span msg])

(defn select-anchor-renderer [props option-props ctx]
  (case @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/type])
    :entity [select-error-cmp "Only fiddle type `query` is supported for select options"]
    :blank [select-error-cmp "Only fiddle type `query` is supported for select options"]
    :query (if (= :db.cardinality/many (:hypercrud.browser/data-cardinality ctx))
             [select-anchor-renderer' props option-props ctx]
             [select-error-cmp "Tuples and scalars are unsupported for select options. Please fix your options query to return a relation or collection"])
    ; default
    [select-error-cmp "Only fiddle type `query` is supported for select options"]))

(let [dom-value (fn [value]                                 ; nil, kw or eid
                  (if (nil? value) "" (str (:db/id value))))]
  (defn select
    ([props ctx]
     (let [link+ (data/select+ ctx :options nil (:hypercrud.browser/path ctx))]
       (either/branch link+ select-error-cmp #(select % props ctx))))
    ([options-link props ctx]
     {:pre [options-link ctx]}
     (either/branch
       (link/eval-hc-props @(r/fmap :hypercrud/props options-link) ctx)
       (fn [e] [(ui-error/error-comp ctx) e])
       (fn [hc-props]
         (let [entity (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]) ; how can this be loading??
               disabled (or (boolean (:read-only props))
                            @(r/fmap nil? entity)           ; no value at all
                            (not @(r/fmap controls/writable-entity? entity)))
               option-props {:disabled disabled}
               props (-> {:on-change (r/partial controls/entity-change! ctx)}
                         (merge props hc-props)
                         (assoc :value @(r/fmap dom-value (:hypercrud.browser/data ctx))))
               ctx (assoc ctx
                     :hypercrud.ui/display-mode (r/track identity :hypercrud.browser.browser-ui/user)
                     :user-renderer (r/partial select-anchor-renderer props option-props))]
           [hyperfiddle.ui/ui-from-link options-link ctx (select-keys props [:class])]))))))
