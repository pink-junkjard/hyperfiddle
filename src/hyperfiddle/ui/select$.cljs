(ns hyperfiddle.ui.select$                                  ; WARNING: Namespace hyperfiddle.ui.select clashes with var hyperfiddle.ui/select
  (:require
    [cats.core :refer [mlet return]]
    [cats.monad.either :as either]
    [contrib.ct :refer [unwrap]]
    [contrib.datomic-tx]
    [contrib.eval]
    [contrib.reactive :as r]
    [contrib.reader]
    [contrib.string :refer [blank->nil]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hypercrud.types.Entity :refer [entity?]]
    [hyperfiddle.data :as data]
    [hyperfiddle.ui.util :refer [writable-entity? entity-change->tx]]
    [taoensso.timbre :as timbre]))

(defn field-label [v]
  (if (instance? cljs.core/Keyword v)
    (name v)                                                ; A sensible default for userland whose idents usually share a long namespace.
    (str v)))

(defn option-label [data ctx]
  (->> (map (fn [data field]
              (->> (::field/children field)
                   (mapv (fn [{f ::field/get-value}]
                           (field-label (f data))))))
            data
            @(r/fmap ::field/children (:hypercrud.browser/field ctx)))
       (apply concat)
       (interpose ", ")
       (remove nil?)
       (apply str)))

(defn select-anchor-renderer' [props option-props ctx]
  ; hack in the selected value if we don't have options hydrated?
  ; Can't, since we only have the #DbId hydrated, and it gets complicated with relaton vs entity etc
  (let [no-options? @(r/fmap empty? (:hypercrud.browser/data ctx))
        props (-> props
                  (update :on-change (fn [on-change]
                                       (fn [e]
                                         (let [select-value (blank->nil (.-target.value e))
                                               id (if select-value
                                                    (let [v (unwrap #(timbre/warn %) (contrib.reader/read-edn-string+ select-value))
                                                          legacy-tempid (and (integer? v) (< v 0))]
                                                      (if legacy-tempid (str v) v)))]
                                           (on-change id)))))
                  ; Don't disable :select if there are options, we may want to see them. Make it look :disabled but allow the click
                  (update :disabled #(or % no-options?))
                  (update :class #(str % (if (:disabled option-props) " disabled"))))
        label-fn (contrib.eval/ensure-fn (:option-label props option-label))
        option-value-fn (fn [row]
                          (let [element (if (vector? row) (first row) row)] ; inspect datalog
                            (pr-str (contrib.datomic-tx/smart-identity element))))]
    [:select.ui (dissoc props :option-label)                ; value
     ; .ui is because options are an iframe and need the pink box
     (conj
       (->> @(:hypercrud.browser/data ctx)
            (mapv (juxt option-value-fn #(label-fn % ctx)))
            (sort-by second)
            (map (fn [[id label]]
                   [:option (assoc option-props :key (str id) :value id) label])))
       [:option (assoc option-props :key :blank :value "") "--"])]))

(defn select-error-cmp [msg]
  [:span msg])

(defn select-anchor-renderer [props option-props val ctx props2]
  (case @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/type])
    :entity [select-error-cmp "Only fiddle type `query` is supported for select options"]
    :blank [select-error-cmp "Only fiddle type `query` is supported for select options"]
    :query (if (= :db.cardinality/many @(r/fmap ::field/cardinality (:hypercrud.browser/field ctx)))
             [select-anchor-renderer' props option-props ctx]
             [select-error-cmp "Tuples and scalars are unsupported for select options. Please fix your options query to return a relation or collection"])
    ; default
    [select-error-cmp "Only fiddle type `query` is supported for select options"]))

(defn compute-disabled [ctx props]
  (let [entity (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])] ; how can this be loading??
    (or (boolean (:read-only props))
        @(r/fmap nil? entity)                               ; no value at all
        (not @(r/fmap writable-entity? entity)))))

(let []
  (defn select "This arity should take a selector string (class) instead of Right[Reaction[Link]], blocked on removing path backdoor"
    [val ctx props]
    {:pre [ctx]}
    (assert (:options props) "select: :options prop is required")
    (-> (mlet [options-ref (data/select+ ctx :hf/iframe (keyword (:options props)))] ; coerce somewhere else tho
          (return
            (let [default-props {:on-change (r/comp (r/partial context/with-tx! ctx)
                                                    (r/partial entity-change->tx ctx))}
                  props (-> (merge default-props props)
                            (assoc :value (str (context/id ctx))))
                  props (-> (select-keys props [:class])
                            (assoc :user-renderer (r/partial select-anchor-renderer props {:disabled (compute-disabled ctx props)})))
                  ctx (assoc ctx
                        :hypercrud.ui/display-mode (r/track identity :hypercrud.browser.browser-ui/user))]
              [hyperfiddle.ui/ui-from-link options-ref ctx props])))
        (either/branch select-error-cmp identity))))
