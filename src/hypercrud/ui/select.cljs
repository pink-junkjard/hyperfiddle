(ns hypercrud.ui.select
  (:require [cats.core :refer [fmap sequence]]
            [cats.monad.either :as either]
            [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.try :refer [try-either]]
            [hypercrud.browser.core :as browser]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.error :as ui-error]))


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
    [:select (dissoc props :label-fn)
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
                  (update :disabled #(or % no-options?)))
        label-fn (:label-fn props label-fn)]
    [:select.select props
     (conj
       (->> @(:relations ctx)
            (mapv (juxt (comp :db/id first) #(label-fn % ctx)))
            (sort-by second)
            (map (fn [[id label]]
                   [:option {:key (str id) :value id} label])))
       [:option {:key :blank :value ""} "--"])]))

(defn select-error-cmp [msg]
  [:span msg])

(defn select-anchor-renderer [props ctx-options]
  (case @(r/cursor (:hypercrud.browser/fiddle ctx-options) [:fiddle/type])
    :entity [select-error-cmp "Only fiddle type `query` is supported for select options"]
    :blank [select-error-cmp "Only fiddle type `query` is supported for select options"]
    :query (if (:relations ctx-options)
             [select-anchor-renderer' props ctx-options]
             [select-error-cmp "Tuples and scalars are unsupported for select options. Please fix your options query to return a relation or collection"])
    ; default
    [select-error-cmp "Only fiddle type `query` is supported for select options"]))

(def always-user (atom :user))

(let [on-change (fn [ctx id]
                  ((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) id)))]
  (defn select* [options-link props ctx]
    (either/branch
      (link/eval-hc-props (:hypercrud/props options-link) ctx)
      (fn [e] [(ui-error/error-comp ctx) e])
      (fn [hc-props]
        (let [props (merge hc-props
                           ; normalize value for the dom - nil, kw or eid
                           {:value (if (nil? @(:value ctx))
                                     ""
                                     (str (:db/id @(:value ctx))))
                            :on-change (r/partial on-change ctx) ; reconstruct the typed value
                            :disabled (:read-only props)})]
          [browser/ui options-link (assoc ctx
                                     :hypercrud.ui/display-mode always-user
                                     :user-renderer (r/partial select-anchor-renderer props))])))))
