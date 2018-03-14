(ns hypercrud.ui.select
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [hypercrud.browser.core :as browser]
            [hypercrud.client.tx :as tx]
            [hypercrud.util.non-fatal :refer [try-either]]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.browser.result :as result]))


(defn default-label-renderer [v ctx]
  (cond
    (instance? cljs.core/Keyword v) (name v)
    :else (str v))
  #_(condp (fn [x c] (instance? c x)) v
      cljs.core/Keyword (name v)
      (str v)))

(defn build-label [relation ctx]
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
       (cats/sequence)
       (cats/fmap (fn [labels]
                    (->> labels
                         (interpose ", ")
                         (remove nil?)
                         (apply str))))))

(defn select-boolean* [value props ctx]
  (let [props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
               :value (if (nil? value) "" (str value))
               ;; reconstruct the typed value
               :on-change #(let [v (case (.-target.value %)
                                     "" nil
                                     "true" true
                                     "false" false)]
                             ((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) v)))
               :disabled (if (:read-only props) true false)}]
    [:select props
     [:option {:key true :value "true"} "True"]
     [:option {:key false :value "false"} "False"]
     [:option {:key :nil :value ""} "--"]]))

(defn select-anchor-renderer' [props ctx]
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
                  (update :disabled #(or % no-options?)))]
    [:select.select props
     (conj
       (->> @(:relations ctx)
            (mapv (fn [relation]
                    (let [label (-> (build-label relation ctx)
                                    ; It's perfectly possible to properly report this error properly upstream.
                                    (either/branch (fn [e] (pr-str e)) identity))]
                      [(:db/id (first relation)) label])))
            (sort-by second)
            (map (fn [[id label]]
                   [:option {:key (str id) :value id} label])))
       [:option {:key :blank :value ""} "--"])]))

(defn select-error-cmp [msg]
  [:span msg])

(defn select-anchor-renderer [props ctx]
  (case @(reactive/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/type])
    :entity [select-error-cmp "Only fiddle type `query` is supported for select options"]
    :blank [select-error-cmp "Only fiddle type `query` is supported for select options"]
    :query (-> (result/with-query-relations ctx)
               (either/branch
                 (fn [e] (throw e))                         ; this exception is always embedded in a safe-user-renderer, so it surfaces safely
                 (fn [ctx-options]
                   (if (:relations ctx-options)
                     (fn [ctx-cell]
                       [select-anchor-renderer' props ctx-options])
                     (constantly
                       [select-error-cmp
                        "Tuples and scalars are unsupported for select options. Please fix your options query to return a relation or collection"])))))
    ; default
    [select-error-cmp "Only fiddle type `query` is supported for select options"]))

(def always-user (atom :user))

(defn anchor->select [props anchor ctx]
  (let [renderer (reactive/partial select-anchor-renderer props)]
    [browser/ui anchor (assoc ctx
                         :hypercrud.ui/display-mode always-user
                         :user-renderer renderer)]))

(let [on-change (fn [ctx id]
                  ((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) id)))]
  (defn select* [options-anchor props ctx]
    (let [props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
                 :value (cond
                          (nil? @(:value ctx)) ""
                          :else (str (:db/id @(:value ctx))))

                 ;; reconstruct the typed value
                 :on-change (reactive/partial on-change ctx)
                 :disabled (:read-only props)}]
      [anchor->select props options-anchor ctx])))
