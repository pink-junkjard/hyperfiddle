(ns hyperfiddle.ui.controls
  (:refer-clojure :exclude [boolean keyword long])
  (:require
    [clojure.set :as set]
    [contrib.datomic-tx :as tx]
    [contrib.reactive :as r]
    [contrib.string :refer [empty->nil]]
    [contrib.ui]                                            ; avoid collisions
    [contrib.ui.input :as input :refer [adapt-props]]
    [contrib.ui.recom-date :refer [recom-date]]
    [hypercrud.browser.context :as context]

    ; This file is the hyperfiddle adapter for the controls, it has to generate
    ; datomic edits and knows about entity dbid. Perhaps the entity-attribute should be
    ; passed instead of the value?
    ))


(defn entity-change->tx [ctx value]
  (let [value (empty->nil value)                            ; safe for non-strings
        entity @(get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])
        attribute @(context/hydrate-attribute ctx (:hypercrud.browser/attribute ctx))]
    (tx/update-entity-attr entity attribute value)))

(defn entity-change! [ctx value]
  ; Sometimes value is partialed, sometimes not, depends on the widget's change! interface which is inconsistent
  (context/with-tx! ctx (entity-change->tx ctx value)))

(defn writable-entity? [entity-val]
  ; If the db/id was not pulled, we cannot write through to the entity
  (cljs.core/boolean (:db/id entity-val)))

(defn ^:export keyword [ref props ctx]
  (let [props (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))
        on-change! (r/partial entity-change! ctx)]
    [input/keyword-input* @ref on-change! props]))

(defn ^:export string [ref props ctx]
  (let [props (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))
        on-change! (r/partial entity-change! ctx)]
    [input/input* @ref on-change! props]))

(defn ^:export long [ref props ctx]
  (let [props (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))
        on-change! (r/partial entity-change! ctx)]
    [input/validated-input @ref on-change!
     #(let [v (js/parseInt % 10)] (if (integer? v) v nil))
     (fnil str "")
     #(or (= "" %) (integer? (js/parseInt % 10)))
     props]))

(defn ^:export boolean [ref props ctx]
  ; wrapper div lets us style block while constraining checkbox clickable surface area inline
  [:div (let [props (adapt-props props)] (update props :class #(str % (if (:disabled props) " disabled"))))
   [contrib.ui/easy-checkbox "" @ref
    (r/partial entity-change! ctx (not @ref))
    (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))]])

(defn ^:export tristate-boolean [ref props ctx]
  (letfn [(adapter [e]
            (case (.-target.value e)
              "" nil
              "true" true
              "false" false))
          (change! [ctx v]
            (entity-change! ctx (adapter v)))]
    (let [option-props {:disabled (or (cljs.core/boolean (:read-only props))
                                      (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))))}]
      [:select (-> (dissoc props :label-fn)
                   (merge {:value (if (nil? @ref) "" (str @ref))
                           :on-change (r/partial change! ctx)}))
       [:option (assoc option-props :key true :value "true") "True"]
       [:option (assoc option-props :key false :value "false") "False"]
       [:option (assoc option-props :key :nil :value "") "--"]])))

(defn ^:export dbid [ref props ctx]
  [input/id-input @ref
   (r/partial entity-change! ctx)
   (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))])

(defn ^:export instant [ref props ctx]
  [recom-date @ref
   (r/partial entity-change! ctx)
   (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))])

(defn- code-mode [& [mode static-props]]
  (fn [ref props ctx]
    (let [props (-> static-props
                    (merge props)
                    (update :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))))))
          control (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                    :hyperfiddle.ui.layout/block contrib.ui/code
                    :hyperfiddle.ui.layout/table contrib.ui/code-inline-block)]
      [control @ref (r/partial entity-change! ctx)
       (cond-> props
               mode (assoc :mode mode))])))

(letfn [(clojure-editor [parinfer]
          (code-mode "clojure" {:parinfer parinfer}))]
  (defn ^:export code [ref props ctx]
    @(->> (:hyperfiddle.ide/user ctx)
          (contrib.reactive/fmap :hyperfiddle.ide/parinfer)
          (contrib.reactive/fmap clojure-editor))))
(def ^:export css (code-mode "css"))

(defn ^:export markdown-editor [ref props ctx]              ; This is legacy; :mode=markdown should be bound in userland
  (let [widget (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                 :hyperfiddle.ui.layout/block contrib.ui/code
                 :hyperfiddle.ui.layout/table contrib.ui/code-inline-block)]
    ;code has backwards args - props first
    [widget @ref (r/partial entity-change! ctx)
     (-> props
         (update :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))
         (assoc :mode "markdown" :lineWrapping true))]))

(defn ^:export edn-many [ref props ctx]
  (letfn [(change! [ctx value user-val]
            (let [user-val (set user-val)
                  rets (set/difference value user-val)
                  adds (set/difference user-val value)]
              (context/with-tx! ctx (tx/edit-entity @(r/fmap :db/id (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))
                                                    (:hypercrud.browser/attribute ctx)
                                                    rets adds))))]
    (let [valueType @(context/hydrate-attribute ctx (:hypercrud.browser/attribute ctx) :db/valueType :db/ident)
          value (set (if (= valueType :db.type/ref) (map :db/id @ref) @ref))
          widget (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                   :hyperfiddle.ui.layout/block contrib.ui/edn
                   :hyperfiddle.ui.layout/table contrib.ui/edn-inline-block)]
      [widget value
       (r/partial change! ctx value)
       (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))])))

(defn ^:export edn [ref props ctx]
  (let [widget (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                 :hyperfiddle.ui.layout/table contrib.ui/edn-inline-block
                 :hyperfiddle.ui.layout/block contrib.ui/edn)]
    [widget @ref
     (r/partial entity-change! ctx)
     (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))]))

;(defn textarea* [{:keys [value on-change] :as props}]       ; unused
;  (let [on-change #(let [newval (.. % -target -value)]
;                     (on-change [value] [newval]))]
;    [:textarea (assoc props :on-change on-change)]))
