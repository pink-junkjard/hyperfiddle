(ns hyperfiddle.ui.controls
  (:refer-clojure :exclude [boolean keyword long])
  (:require
    [cats.monad.either :refer [branch]]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.ui :refer [debounced]]                         ; avoid collisions
    [contrib.ui.recom-date :refer [recom-date]]
    [hypercrud.browser.context :as context]
    [hyperfiddle.data :as data]
    [hyperfiddle.tempid :refer [tempid? underlying-tempid stable-relation-key smart-entity-identifier]]
    [hyperfiddle.ui.select$ :refer [select]]
    [hyperfiddle.ui.util :refer [entity-props readonly->disabled on-change->tx writable-entity?]]))


(defn ^:export keyword [val ctx & [props]]
  (let [props (-> (entity-props val props ctx)
                  readonly->disabled)]
    [debounced props contrib.ui/keyword]))

(defn ^:export string [val ctx & [props]]
  (let [props (-> (entity-props val props ctx)
                  readonly->disabled)]
    [debounced props contrib.ui/text #_contrib.ui/textarea]))

(defn ^:export long [val ctx & [props]]
  (let [props (-> (entity-props val props ctx)
                  readonly->disabled)]
    [debounced props contrib.ui/long]))

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

(defn id-label [ctx val]
  (pr-str (smart-entity-identifier ctx val)))

(defn ^:export ref [val ctx & [props]]
  (cond
    (:options props) [select val ctx props]
    :else [:div
           [:div.input
            (or (if-let [self (data/select-here ctx :hf/edit)]
                  (if val
                    [hyperfiddle.ui/ui-from-link self ctx props (id-label ctx val)]))
                (id-label ctx val))]

           (if-let [link (data/select-here ctx :hf/affix)]
             [hyperfiddle.ui/ui-from-link link ctx props "affix"])

           (if-let [link (data/select-here ctx :hf/remove)]
             [hyperfiddle.ui/ui-from-link link ctx props "remove"])

           (if-let [link (data/select-here ctx :hf/detach)]
             [hyperfiddle.ui/ui-from-link link ctx props "detach"])]))

(defn ^:export id-or-ident [val ctx & [props]]
  ; id control uses links from parent ctx (parent ref and parent path)
  ; select-here does not match :hf/edit since it is in the parent ref position
  (let [ctx (:hypercrud.browser/parent ctx)]
    [:div
     [:div.input
      ; pr-str here to disambiguate `"tempid"` from `17592186046396` and `:gender/male`
      (or (if-let [self (data/select-here ctx :hf/edit)]
            (if val
              [hyperfiddle.ui/ui-from-link self ctx props (pr-str val)]))
          (pr-str val))]
     (if-let [link (data/select-here ctx :hf/affix)]
       [hyperfiddle.ui/ui-from-link link ctx props "affix"])

     (if-not (underlying-tempid ctx val)
       (if-let [link (data/select-here ctx :hf/remove)]
         [hyperfiddle.ui/ui-from-link link ctx props "remove"]))

     (if-let [link (data/select-here ctx :hf/detach)]
       [hyperfiddle.ui/ui-from-link link ctx props "detach"])

     (let [related (data/select-all ctx :hf/rel)]
       (->> (r/track identity related)
            (r/unsequence (r/partial stable-relation-key ctx))
            (map (fn [[rv k]]
                   ^{:key k}                                ; Use the userland class as the label (ignore hf/rel)
                   [hyperfiddle.ui/ui-from-link rv ctx props]))
            doall))]))

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
    [debounced props (code-comp ctx)]))

(defn ^:export css [val ctx & [props]]
  (let [props (-> (entity-props val props ctx)
                  (update :mode #(or % "css")))]
    [debounced props (code-comp ctx)]))

(defn ^:export markdown-editor [val ctx & [props]]          ; This is legacy; :mode=markdown should be bound in userland
  (let [props (-> (entity-props val props ctx)
                  (assoc :mode "markdown"
                         :lineWrapping true))]
    [debounced props (code-comp ctx)]))

(defn- edn-comp [ctx]
  (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
    :hyperfiddle.ui.layout/block contrib.ui/cm-edn
    :hyperfiddle.ui.layout/table contrib.ui/cm-edn-inline-block))

(defn ^:export edn-many [val ctx & [props]]
  (let [valueType @(context/hydrate-attribute ctx (last (:hypercrud.browser/path ctx)) :db/valueType :db/ident)
        val (set (if (= valueType :db.type/ref) (map (r/partial smart-entity-identifier ctx) val) val))
        props (entity-props val props ctx)]
    [debounced props (edn-comp ctx)]))

(defn ^:export edn [val ctx & [props]]
  (let [props (entity-props val props ctx)]
    [debounced props (edn-comp ctx)]))
