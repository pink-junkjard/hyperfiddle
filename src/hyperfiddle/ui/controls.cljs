(ns hyperfiddle.ui.controls
  (:refer-clojure :exclude [boolean keyword long])
  (:require
    [contrib.reactive :as r]
    [contrib.ui :refer [debounced optimistic-updates]]      ; avoid collisions
    [contrib.ui.input :as input]
    [contrib.ui.recom-date :refer [recom-date]]
    [hypercrud.browser.context :as context]
    [hyperfiddle.ui.select$ :refer [select]]
    [hyperfiddle.ui.util :refer [entity-props readonly->disabled on-change->tx writable-entity?]]))


(defn ^:export keyword [val ctx & [props]]
  (let [props (-> (entity-props val props ctx)
                  readonly->disabled)]
    [optimistic-updates props debounced input/keyword]))

(defn ^:export string [val ctx & [props]]
  (let [props (-> (entity-props val props ctx)
                  readonly->disabled)]
    [optimistic-updates props debounced input/text #_contrib.ui/textarea]))

(defn ^:export long [val ctx & [props]]
  (let [props (-> (entity-props val props ctx)
                  readonly->disabled)]
    [optimistic-updates props debounced input/long]))

(defn ^:export boolean [val ctx & [props]]
  [:div (let [props (readonly->disabled props)]
          (update props :class #(str % (if (:disabled props) " disabled"))))
   (let [props (-> (entity-props props ctx)
                   (readonly->disabled)
                   (update :on-change #(r/partial % val))   ; need to adapt (fn [n]) to (fn [o n]) when no optimistic updates
                   (assoc :checked val))]
     [contrib.ui/easy-checkbox props])])

(let [adapter (fn [e]
                (case (.-target.value e)
                  "" nil
                  "true" true
                  "false" false))]
  (defn ^:export tristate-boolean [val ctx & [props]]
    (let [option-props (-> (readonly->disabled props)
                           (update :disabled #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))))))]
      [:select (-> (dissoc props :label-fn)
                   (assoc :value (if (nil? val) "" (str val))
                          :on-change (r/comp
                                       (r/partial context/with-tx! ctx)
                                       (r/partial on-change->tx ctx val)
                                       adapter)))
       [:option (assoc option-props :key true :value "true") "True"]
       [:option (assoc option-props :key false :value "false") "False"]
       [:option (assoc option-props :key :nil :value "") "--"]])))

(defn ^:export ref [val ctx & [props]]
  (if (:options props)
    [select val ctx props]
    (let [props (-> (entity-props val #_(or (:db/ident val) (:db/id val)) props ctx)
                    readonly->disabled)]
      [optimistic-updates props debounced input/edn])))

(defn ^:export dbid [val ctx & [props]]
  (let [props (-> (entity-props (:db/id val) props ctx)
                  readonly->disabled)]
    [optimistic-updates props debounced input/edn]))

(defn ^:export instant [val ctx & [props]]
  (let [props (-> (entity-props val props ctx)
                  readonly->disabled)]
    [recom-date props]))

(defn- code-comp [ctx]
  (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
    :hyperfiddle.ui.layout/block contrib.ui/code
    :hyperfiddle.ui.layout/table contrib.ui/code-inline-block))

(defn ^:export code [val ctx & [props]]
  (let [props (-> (entity-props val props ctx)
                  (update :mode #(or % "clojure"))
                  (assoc :parinfer @(r/fmap :hyperfiddle.ide/parinfer (:hyperfiddle.ide/user ctx))))]
    [optimistic-updates props debounced (code-comp ctx)]))

(defn ^:export css [val ctx & [props]]
  (let [props (-> (entity-props val props ctx)
                  (update :mode #(or % "css")))]
    [optimistic-updates props debounced (code-comp ctx)]))

(defn ^:export markdown-editor [val ctx & [props]]              ; This is legacy; :mode=markdown should be bound in userland
  (let [props (-> (entity-props val props ctx)
                  (assoc :mode "markdown"
                         :lineWrapping true))]
    [optimistic-updates props debounced (code-comp ctx)]))

(defn- edn-comp [ctx]
  (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
    :hyperfiddle.ui.layout/block contrib.ui/edn
    :hyperfiddle.ui.layout/table contrib.ui/edn-inline-block))

(defn ^:export edn-many [val ctx & [props]]
  (let [valueType @(context/hydrate-attribute ctx (last (:hypercrud.browser/path ctx)) :db/valueType :db/ident)
        val (set (if (= valueType :db.type/ref) (map :db/id val) val))
        props (entity-props val props ctx)]
    [optimistic-updates props debounced (edn-comp ctx)]))

(defn ^:export edn [val ctx & [props]]
  (let [props (entity-props val props ctx)]
    [optimistic-updates props debounced (edn-comp ctx)]))
