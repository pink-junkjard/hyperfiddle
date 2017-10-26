(ns hypercrud.ui.select
  (:require [cats.core :as cats]
            [cats.monad.either :as either :refer-macros [try-either]]
            [hypercrud.browser.core :as browser]
            [hypercrud.client.tx :as tx]
            [reagent.core :as reagent]))


(defn default-label-renderer [v ctx]
  (cond
    (instance? cljs.core/Keyword v) (name v)
    :else (str v))
  #_(condp (fn [x c] (instance? c x)) v
      cljs.core/Keyword (name v)
      (str v)))

(defn build-label [ordered-fes relation param-ctx]
  (->> ordered-fes
       (mapcat (fn [fe]
                 (->> (-> fe :find-element/form :form/field)
                      (mapv (fn [{:keys [:field/attribute]}]
                              ; Custom label renderers? Can't use the attribute renderer, since that
                              ; is how we are in a select options in the first place.
                              (let [value (get-in relation [(:find-element/name fe) attribute])
                                    renderer (or (-> param-ctx :fields attribute :label-renderer) default-label-renderer)]
                                (try-either (renderer value param-ctx))))))))
       (cats/sequence)
       (cats/fmap (fn [labels]
                    (->> labels
                         (interpose ", ")
                         (apply str))))))

(defn select-boolean* [value props param-ctx]
  (let [props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
               :value (if (nil? value) "" (str value))
               ;; reconstruct the typed value
               :on-change #(let [v (case (.-target.value %)
                                     "" nil
                                     "true" true
                                     "false" false)]
                             ((:user-with! param-ctx) (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) v)))
               :disabled (if (:read-only props) true false)}]
    [:select props
     [:option {:key true :value "true"} "True"]
     [:option {:key false :value "false"} "False"]
     [:option {:key :nil :value ""} "--"]]))

(defn select-anchor-renderer [props result ordered-fes anchors param-ctx]
  ; hack in the selected value if we don't have options hydrated?
  ; Can't, since we only have the #DbId hydrated, and it gets complicated with relaton vs entity etc
  (let [no-options? (empty? result)
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
       (->> result
            (mapv (fn [relation]
                    (let [entity (get relation (:find-element/name (first ordered-fes)))
                          label (-> (build-label ordered-fes relation param-ctx)
                                    ; It's perfectly possible to properly report this error properly upstream.
                                    (either/branch (fn [e] (pr-str e)) identity))]
                      [(:db/id entity) label])))
            (sort-by second)
            (map (fn [[id label]]
                   [:option {:key (str id) :value id} label])))
       [:option {:key :blank :value ""} "--"])]))

(def always-user (atom :user))

(defn anchor->select [props anchor param-ctx]
  (let [renderer (reagent/partial select-anchor-renderer props)]
    [browser/ui anchor (assoc param-ctx
                         :display-mode always-user
                         :user-renderer renderer)]))

(let [on-change (fn [param-ctx id]
                  ((:user-with! param-ctx) (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) id)))]
  (defn select* [value options-anchor props param-ctx]
    (let [props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
                 :value (cond
                          (nil? value) ""
                          :else (str (:db/id value)))

                 ;; reconstruct the typed value
                 :on-change (reagent/partial on-change param-ctx)
                 :disabled (:read-only props)}]
      [anchor->select props options-anchor param-ctx])))
