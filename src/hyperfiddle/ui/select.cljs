(ns hyperfiddle.ui.select
  (:require [cats.core :as cats :refer [fmap]]
            [cats.monad.either :as either]
            [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.try$ :refer [try-either]]
            [hypercrud.browser.core :as browser]
            [hypercrud.browser.field :as field]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.error :as ui-error]
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
  (let [label'
        (->> (map (fn [data field]
                    (->> (::field/children field)
                         (mapv (fn [{:keys [::field/get-value ::field/path-segment]}]
                                 ; Custom label renderers? Can't use the attribute renderer, since that
                                 ; is how we are in a select options in the first place.
                                 (let [renderer (get-in ctx [:fields path-segment :label-renderer] default-label-renderer)]
                                   (try-either (renderer (get-value data) ctx)))))))
                  data
                  @(:hypercrud.browser/fields ctx))
             (apply concat)
             cats/sequence
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
                  (update :disabled #(or % no-options?)))
        label-fn (:label-fn props label-fn)]
    [:select.ui (dissoc props :label-fn)
     ; .ui is because options are an iframe and need the pink box
     (conj
       (->> @(:hypercrud.browser/data ctx)
            (mapv (juxt (comp :db/id first) #(label-fn % ctx)))
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

(def always-user (atom :hypercrud.browser.browser-ui/user))

(let [dom-value (fn [value]                                 ; nil, kw or eid
                  (if (nil? value) "" (str (:db/id value))))]
  (defn select [props ctx]
    (let [options-link @(r/track link/rel->link :options ctx)]
      (either/branch
        (link/eval-hc-props (:hypercrud/props options-link) ctx)
        (fn [e] [(ui-error/error-comp ctx) e])
        (fn [hc-props]
          (let [entity (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]) ; how can this be loading??
                option-props {:disabled (or (boolean (:read-only props))
                                            @(r/fmap nil? entity) ; no value at all
                                            (not @(r/fmap controls/writable-entity? entity)))}
                props (-> {:on-change (r/partial controls/entity-change! ctx)}
                          (merge props hc-props)
                          (assoc :value @(r/fmap dom-value (:hypercrud.browser/data ctx))))
                f (r/partial select-anchor-renderer props option-props)]
            [browser/ui options-link (assoc ctx
                                       :hypercrud.ui/display-mode always-user
                                       :user-renderer f)
             (:class props)]))))))
