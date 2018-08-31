(ns hyperfiddle.ui.hyper-controls
  (:require
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.ui :refer [debounced]]
    [contrib.ui.input :as input]
    [contrib.ui.tooltip :refer [tooltip-thick]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hyperfiddle.data :as data]
    #_[hyperfiddle.ui]
    [hyperfiddle.ui.util :refer [readonly->disabled writable-entity?]]
    [hyperfiddle.ui.docstring :refer [semantic-docstring]]))


(defn attribute-label [_ ctx & [props]]
  (when-let [label (some->> (:hypercrud.browser/field ctx)
                            (r/fmap ::field/label)
                            deref)]
    (let [help-md (semantic-docstring ctx)]
      [tooltip-thick (if help-md
                       [:div.hyperfiddle.docstring [contrib.ui/markdown help-md]])
       [:label props label (if help-md [:sup "†"])]])))

(defn entity-label [_ ctx & [props]]
  [:div #_(fragment)
   [attribute-label _ props ctx]
   (->> (data/select-all ctx :hyperfiddle/new)
        (r/track identity)
        (r/unsequence :db/id)
        (map (fn [[rv k]]
               ^{:key k}
               [hyperfiddle.ui/ui-from-link rv ctx props]))
        doall)])

(defn magic-new-head [_ ctx & [props]]
  (let [state (r/cursor (:hyperfiddle.ui.form/state ctx) [:hyperfiddle.ui.form/magic-new-a])]
    ;(println (str/format "magic-new-head: %s , %s , %s" @state (pr-str @entity)))
    [input/keyword (-> (assoc props
                         :placeholder ":task/title"
                         :value @state
                         :on-change (r/partial reset! state))
                       readonly->disabled)]))

(letfn [(change! [ctx state v]
          (context/with-tx! ctx [[:db/add @(r/fmap :db/id (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])) @state v]]))]
  (defn magic-new-body [val ctx & [props]]
    (let [read-only (r/fmap (comp not writable-entity?) (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))
          state (r/cursor (:hyperfiddle.ui.form/state ctx) [:hyperfiddle.ui.form/magic-new-a])
          props (-> (assoc props
                      :on-change (r/partial change! ctx state)
                      :read-only (let [_ [@state @read-only]] ; force reactions
                                   (or (nil? @state) @read-only))
                      :placeholder (pr-str "mow the lawn"))
                    readonly->disabled)]
      ;(println (str/format "magic-new-body: %s , %s , %s" @state @read-only (pr-str @entity)))
      ; Uncontrolled widget on purpose i think
      ; Cardinality many is not needed, because as soon as we assoc one value,
      ; we dispatch through a proper typed control
      [debounced props input/edn])))
