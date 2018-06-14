(ns hyperfiddle.ui.select
  (:require [cats.core :refer [fmap sequence]]
            [cats.monad.either :as either]
            [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.try :refer [try-either]]
            [hypercrud.browser.core :as browser]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.error :as ui-error]
            [hypercrud.browser.context :as context]
            [hyperfiddle.ui.controls :as controls]))


(defn default-label-renderer [v ctx]
  (if (instance? cljs.core/Keyword v)
    (name v)                                                ; A sensible default for userland whose idents usually share a long namespace.
    (str v))
  #_(condp (fn [x c] (instance? c x)) v
      cljs.core/Keyword (name v)
      (str v)))

(defn label-fn [relation ctx]                               ; It's perfectly possible to properly report this error properly upstream. (later - is it?)
  ; This whole thing is legacy, migrate it to link props
  (let [label'
        (->> (map (fn [cell-data fe]
                    (->> (:fields fe)
                         (mapv (fn [field {:keys [attribute]}]
                                 ; Custom label renderers? Can't use the attribute renderer, since that
                                 ; is how we are in a select options in the first place.
                                 (let [value ((:cell-data->value field) cell-data)
                                       renderer (get-in ctx [:fields attribute :label-renderer] default-label-renderer)]
                                   (try-either (renderer value ctx)))))))
                  relation
                  @(:hypercrud.browser/ordered-fes ctx))
             (apply concat)
             sequence
             (fmap #(->> % (interpose ", ") (remove nil?) (apply str))))]
    (either/branch label' pr-str identity)))

(defn select-anchor-renderer' [props option-props ctx]
  ; hack in the selected value if we don't have options hydrated?
  ; Can't, since we only have the #DbId hydrated, and it gets complicated with relaton vs entity etc
  (let [no-options? (empty? @(:relations ctx))
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
    [:select (dissoc props :label-fn)
     (conj
       (->> @(:relations ctx)
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
    :query (if (:relations ctx)
             [select-anchor-renderer' props option-props ctx]
             [select-error-cmp "Tuples and scalars are unsupported for select options. Please fix your options query to return a relation or collection"])
    ; default
    [select-error-cmp "Only fiddle type `query` is supported for select options"]))

(def always-user (atom :hypercrud.browser.browser-ui/user))

(let [on-change (fn [ctx id]                                ;reconstruct the typed value
                  ((:user-with! ctx) (tx/update-entity-attr @(context/entity ctx)
                                                            @(:hypercrud.browser/fat-attribute ctx) id)))
      dom-value (fn [value]                                 ; nil, kw or eid
                  (if (nil? value) "" (str (:db/id value))))]
  (defn select [value ctx props]
    (let [options-link @(r/track link/options-link ctx)]
      (either/branch
        (link/eval-hc-props (:hypercrud/props options-link) ctx)
        (fn [e] [(ui-error/error-comp ctx) e])
        (fn [hc-props]
          (let [entity (context/entity ctx)
                option-props {:disabled (or (boolean (:read-only props))
                                            @(r/fmap nil? entity) ; no value at all
                                            (not @(r/fmap controls/writable-entity? entity)))}
                props (merge props hc-props {:on-change (r/partial on-change ctx)
                                             :value (dom-value value)})
                f (r/partial select-anchor-renderer props option-props)]
            [browser/ui options-link (assoc ctx
                                       :hypercrud.ui/display-mode always-user
                                       :user-renderer f)
             (:class props)]))))))
