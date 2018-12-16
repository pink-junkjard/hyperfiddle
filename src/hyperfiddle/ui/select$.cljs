(ns hyperfiddle.ui.select$                                  ; WARNING: Namespace hyperfiddle.ui.select clashes with var hyperfiddle.ui/select
  (:require
    [cats.core :refer [mlet return]]
    [cats.monad.either :as either]
    [contrib.ct :refer [unwrap]]
    [contrib.eval]
    [contrib.reactive :as r]
    [contrib.reader]
    [contrib.string :refer [blank->nil]]
    [datascript.parser :refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hyperfiddle.data :as data]
    [hyperfiddle.ui.util :refer [with-entity-change! writable-entity?]]
    [taoensso.timbre :as timbre]))

(defn field-label [v]
  (if (instance? cljs.core/Keyword v)
    (name v)                                                ; A sensible default for userland whose idents usually share a long namespace.
    (str v)))

(defn option-label [row ctx]
  (let [field @(:hypercrud.browser/field ctx)
        {{find :qfind} ::field/query} field]

    ; This logic duplicates ui/columns-relation-product &co
    (->>
      (condp = (type find)
        FindRel
        (mapcat (fn [element field relation]
                  (condp = (type element)
                    Variable [relation]
                    Aggregate [relation]
                    Pull (mapv (fn [{f ::field/get-value}]
                                 (field-label (f relation)))
                               (::field/children field))))
                (:elements find)
                (::field/children field)
                row)
        FindColl
        (condp = (type (:element find))
          Variable [row]
          Aggregate [row]
          Pull (mapv (fn [{f ::field/get-value}]            ; identity
                       (field-label (f row)))
                     (::field/children field)))
        FindTuple [row]                                     ; bug - bad wrapper?
        FindScalar [row])
      (remove nil?)
      (interpose ", ")
      (apply str))))

(defn select-anchor-renderer' [props option-props ctx]
  ; hack in the selected value if we don't have options hydrated?
  ; Can't, since we only have the #DbId hydrated, and it gets complicated with relaton vs entity etc
  (let [is-no-options @(r/fmap empty? (:hypercrud.browser/data ctx))
        props (-> props
                  (update :on-change (fn [on-change]
                                       (fn [e]
                                         (-> (some->> (.-target.value e)
                                                      blank->nil
                                                      contrib.reader/read-edn-string+ ; todo why handle this exception? just throw and call it a day
                                                      (unwrap #(timbre/warn %))) ; instead of terminating on error, the user now transacts a retract
                                             on-change))))
                  (dissoc :disabled)                        ; Use :read-only instead to allow click to expand options
                  (update :read-only #(or % (:disabled props) is-no-options))
                  (update :class #(str % (if (:disabled option-props) " disabled"))))
        label-fn (contrib.eval/ensure-fn (:option-label props option-label))
        option-value-fn (fn [row]
                          (let [element (if (vector? row) (first row) row)] ; inspect datalog
                            (pr-str (or (context/smart-entity-identifier ctx element) element))))]
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

(defn select-view-validated [select-view props option-props val ctx props2]
  (case @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/type])
    :entity [select-error-cmp "Only fiddle type `query` is supported for select options"]
    :blank [select-error-cmp "Only fiddle type `query` is supported for select options"]
    :query (if (= :db.cardinality/many @(r/fmap ::field/cardinality (:hypercrud.browser/field ctx)))
             (let [props (into props (select-keys props2 [:on-click]))]
               [select-view props option-props ctx])
             [select-error-cmp "Tuples and scalars are unsupported for select options. Please fix your options query to return a relation or collection"])
    ; default
    [select-error-cmp "Only fiddle type `query` is supported for select options"]))

(defn compute-disabled [ctx props]
  (let [entity (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])] ; how can this be loading??
    (or (boolean (:disabled props))
        (boolean (:read-only props))                        ; legacy
        ; no value at all
        @(r/fmap nil? entity))))

(defn select "This arity should take a selector string (class) instead of Right[Reaction[Link]], blocked on removing path backdoor"
  [val ctx props]
  {:pre [ctx]}
  (assert (:options props) "select: :options prop is required")
  (-> (mlet [options-ref (data/select+ ctx (keyword (:options props)))] ; coerce somewhere else tho
        (return
          (let [default-props {:on-change (with-entity-change! ctx)}
                props (-> (merge default-props props)
                          (assoc :value (str (context/identify ctx))))
                props (-> (select-keys props [:class])
                          (assoc :user-renderer (r/partial select-view-validated select-anchor-renderer' props {:disabled (compute-disabled ctx props)})))
                ctx (assoc ctx
                      :hypercrud.ui/display-mode (r/track identity :hypercrud.browser.browser-ui/user))]
            [hyperfiddle.ui/ui-from-link options-ref ctx props])))
      (either/branch select-error-cmp identity)))
