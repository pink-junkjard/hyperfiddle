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
    [reagent.core :as reagent]
    [taoensso.timbre :as timbre]))


(defn entity-change->tx [ctx value]
  (let [value (empty->nil value)                            ; safe for non-strings
        entity @(get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])
        attribute @(context/hydrate-attribute ctx (last (:hypercrud.browser/path ctx)))]
    (tx/update-entity-attr entity attribute value)))

(defn entity-change! [ctx value]
  ; Sometimes value is partialed, sometimes not, depends on the widget's change! interface which is inconsistent
  (context/with-tx! ctx (entity-change->tx ctx value)))

(defn writable-entity? [entity-val]
  ; If the db/id was not pulled, we cannot write through to the entity
  (cljs.core/boolean (:db/id entity-val)))

(letfn [(on-change! [ctx os-ref new-value]
          (let [attr-ident (last (:hypercrud.browser/path ctx))
                entity (-> @(get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])
                           (select-keys [:db/id])
                           (assoc attr-ident (empty->nil (last @os-ref))))
                attribute @(context/hydrate-attribute ctx attr-ident)
                tx (tx/update-entity-attr entity attribute (empty->nil new-value))]
            (context/with-tx! ctx tx))
          (swap! os-ref conj new-value))]
  (defn entity-adapter [ctx comp props & args]
    (let [os-ref (r/atom {:value (:value props)
                          :old-values [(:value props)]})]
      (reagent/create-class
        {:reagent-render
         (fn [ctx comp props & args]
           (let [props (-> props
                           (assoc
                             :value @(r/cursor os-ref [:value])
                             :on-change (r/partial
                                          on-change! ctx
                                          ; reagent/cursor, because we need to swap!
                                          (reagent/cursor os-ref [:old-values])))
                           (update :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))))))]
             (into [comp props] args)))
         :component-did-update
         (fn [this]
           (let [[_ ctx comp props & args] (reagent/argv this)
                 b (:value props)
                 {os :old-values a :value} @os-ref]
             (if (= 1 (count os))
               (when (not= a b)
                 (reset! os-ref {:old-values [b]
                                 :value b}))
               (let [[_ [old-update :as rest]] (split-with #(not= % b) os)]
                 (if (= old-update b)                       ; careful do NOT nil pun
                   (swap! os-ref assoc :old-values (vec rest))
                   (when (not= a b)
                     ; this is either not a big deal, e.g. default values have been applied
                     ; or multiple users are editing the same value and changes are probably being lost
                     (timbre/warn "Potential conflicting concurrent edits detected, discarding local state" {:a a :b b :os os})
                     (reset! os-ref {:old-values [b]
                                     :value b})))))))}))))

(defn ^:export keyword [val ctx & [props]]
  (let [props (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))
        on-change! (r/partial entity-change! ctx)]
    [input/keyword-input* val on-change! props]))

(defn ^:export string [val ctx & [props]]
  (let [props (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))
        on-change! (r/partial entity-change! ctx)]
    [input/input* val on-change! props]))

#_(defn ^:export string [val props ctx]
    [entity-adapter ctx input/input* (assoc props :value val)])

(defn ^:export long [val ctx & [props]]
  (let [props (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))
        on-change! (r/partial entity-change! ctx)]
    [input/validated-input val on-change!
     #(let [v (js/parseInt % 10)] (if (integer? v) v nil))
     (fnil str "")
     #(or (= "" %) (integer? (js/parseInt % 10)))
     props]))

(defn ^:export boolean [val ctx & [props]]
  ; wrapper div lets us style block while constraining checkbox clickable surface area inline
  [:div (let [props (adapt-props props)] (update props :class #(str % (if (:disabled props) " disabled"))))
   [contrib.ui/easy-checkbox "" val
    (r/partial entity-change! ctx (not val))
    (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))]])

(defn ^:export tristate-boolean [val ctx & [props]]
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
                   (merge {:value (if (nil? val) "" (str val))
                           :on-change (r/partial change! ctx)}))
       [:option (assoc option-props :key true :value "true") "True"]
       [:option (assoc option-props :key false :value "false") "False"]
       [:option (assoc option-props :key :nil :value "") "--"]])))

(defn ^:export ref [val ctx & [props]]
  [input/edn-input* val #_(or (:db/ident val) (:db/id val))
   (r/partial entity-change! ctx)                           ; f'ed
   (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))])

(defn ^:export dbid [val ctx & [props]]
  [input/id-input val
   (r/partial entity-change! ctx)
   (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))])

(defn ^:export instant [val ctx & [props]]
  [recom-date val
   (r/partial entity-change! ctx)
   (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))])

(defn- code-comp [ctx]
  (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
    :hyperfiddle.ui.layout/block contrib.ui/code
    :hyperfiddle.ui.layout/table contrib.ui/code-inline-block))

(defn ^:export code [val ctx & [props]]
  (let [props (-> props
                  (assoc :value val)
                  (update :mode #(or % "clojure"))
                  (assoc :parinfer @(r/fmap :hyperfiddle.ide/parinfer (:hyperfiddle.ide/user ctx))))]
    [entity-adapter ctx (code-comp ctx) props]))

(defn ^:export css [val ctx & [props]]
  (let [props (-> props
                  (assoc :value val)
                  (update :mode #(or % "css")))]
    [entity-adapter ctx (code-comp ctx) props]))

(defn ^:export markdown-editor [val ctx & [props]]              ; This is legacy; :mode=markdown should be bound in userland
  (let [props (assoc props
                :value val
                :mode "markdown"
                :lineWrapping true)]
    [entity-adapter ctx (code-comp ctx) props]))

(defn- edn-comp [ctx]
  (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
    :hyperfiddle.ui.layout/block contrib.ui/edn
    :hyperfiddle.ui.layout/table contrib.ui/edn-inline-block))

(letfn [(change! [ctx value user-val]
          (let [user-val (set user-val)
                rets (set/difference value user-val)
                adds (set/difference user-val value)]
            (context/with-tx! ctx (tx/edit-entity @(r/fmap :db/id (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))
                                                  (last (:hypercrud.browser/path ctx))
                                                  rets adds))))]
  (defn ^:export edn-many [val ctx & [props]]
    (let [valueType @(context/hydrate-attribute ctx (last (:hypercrud.browser/path ctx)) :db/valueType :db/ident)
          value (set (if (= valueType :db.type/ref) (map :db/id val) val))
          props (-> props
                    (assoc :value value
                           :on-change (r/partial change! ctx value))
                    (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))))))]
      [(edn-comp ctx) props])))

(defn ^:export edn [val ctx & [props]]
  [entity-adapter ctx (edn-comp ctx) (assoc props :value val)])

;(defn textarea* [{:keys [value on-change] :as props}]       ; unused
;  (let [on-change #(let [newval (.. % -target -value)]
;                     (on-change [value] [newval]))]
;    [:textarea (assoc props :on-change on-change)]))
