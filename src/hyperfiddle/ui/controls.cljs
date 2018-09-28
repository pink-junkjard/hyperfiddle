(ns hyperfiddle.ui.controls
  (:refer-clojure :exclude [boolean keyword long])
  (:require
    [cats.monad.either :refer [branch]]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.ui :refer [debounced]]                         ; avoid collisions
    [contrib.ui.recom-date :refer [recom-date]]
    [contrib.ui.tooltip :refer [tooltip-thick]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hyperfiddle.data :as data]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.tempid :refer [tempid? underlying-tempid stable-relation-key smart-entity-identifier]]
    #_[hyperfiddle.ui]
    [hyperfiddle.ui.docstring :refer [semantic-docstring]]
    [hyperfiddle.ui.select$ :refer [select]]
    [hyperfiddle.ui.util :refer [with-entity-change! with-tx! writable-entity?]]))


(defn ^:export keyword [val ctx & [props]]
  (let [props (-> (assoc props
                    :value val
                    :on-change (with-entity-change! ctx))
                  (update :disabled #(or % (not @(r/track writable-entity? ctx)))))]
    [debounced props contrib.ui/keyword]))

(defn ^:export string [val ctx & [props]]
  (let [props (-> (assoc props
                    :value val
                    :on-change (with-entity-change! ctx))
                  (update :disabled #(or % (not @(r/track writable-entity? ctx)))))]
    [debounced props contrib.ui/text #_contrib.ui/textarea]))

(defn ^:export long [val ctx & [props]]
  (let [props (-> (assoc props
                    :value val
                    :on-change (with-entity-change! ctx))
                  (update :disabled #(or % (not @(r/track writable-entity? ctx)))))]
    [debounced props contrib.ui/long]))

(defn ^:export boolean [val ctx & [props]]
  (let [props (update props :disabled #(or % (not @(r/track writable-entity? ctx))))]
    [:div (update props :class #(str % (if (:disabled props) " disabled")))
     (let [props (assoc props
                   :checked (clojure.core/boolean val)
                   :on-change (with-entity-change! ctx))]
       [contrib.ui/easy-checkbox props])]))

(let [adapter (fn [e]
                (case (.-target.value e)
                  "" nil
                  "true" true
                  "false" false))]
  (defn ^:export tristate-boolean [val ctx & [props]]
    (let [option-props (update props :disabled #(or % (not @(r/track writable-entity? ctx))))]
      [:select (-> (dissoc props :label-fn)
                   (assoc :value (if (nil? val) "" (str val))
                          :on-change (r/comp (with-entity-change! ctx) adapter)))
       [:option (assoc option-props :key true :value "true") "True"]
       [:option (assoc option-props :key false :value "false") "False"]
       [:option (assoc option-props :key :nil :value "") "--"]])))

(defn label-with-docs [label help-md props]
  [tooltip-thick (if help-md [:div.hyperfiddle.docstring [contrib.ui/markdown help-md]])
   (let [label-props (select-keys props [:on-click :class])] ; https://github.com/hyperfiddle/hyperfiddle/issues/511
     [:label.hyperfiddle label-props label (if help-md [:sup "†"])])])

(defn cardinality [ctx]
  (let [segment (last (:hypercrud.browser/path ctx))
        attr @(context/hydrate-attribute ctx segment)]
    (some-> attr :db/cardinality :db/ident)))

(defn dbid-label [_ ctx & [props]]
  (let [prompt (some->> (:hypercrud.browser/field ctx) (r/fmap ::field/label) deref)
        parent-ctx (:hypercrud.browser/parent ctx)]
    (apply fragment
           (label-with-docs prompt (semantic-docstring ctx) props)
           ; dbid links are at parent path, but we don't always have a parent #543
           (condp = (some-> parent-ctx cardinality)

             ; :one is handled by the body
             :db.cardinality/one nil

             :db.cardinality/many
             [(if-let [link (data/select-here parent-ctx :hf/affix)]
                [hyperfiddle.ui/ui-from-link link parent-ctx props "affix"])
              (if-let [link (data/select-here parent-ctx :hf/new)]
                [hyperfiddle.ui/ui-from-link link parent-ctx props "new"])]

             ; This hack detects FindCol, which has no parent cardinality but does need the links
             nil [(if-let [link (some-> parent-ctx (data/select-here :hf/affix))]
                    [hyperfiddle.ui/ui-from-link link parent-ctx props "affix"])
                  (if-let [link (some-> parent-ctx (data/select-here :hf/new))]
                    [hyperfiddle.ui/ui-from-link link parent-ctx props "new"])]
             ))))

(defn id-prompt [ctx val]
  (pr-str (smart-entity-identifier ctx val)))

(defn ^:export ref [val ctx & [props]]
  (cond
    (:options props) [select val ctx props]
    :else [:div
           [:div.input
            (or (if-let [self (data/select-here ctx :hf/self)]
                  (if val
                    [hyperfiddle.ui/ui-from-link self ctx props (id-prompt ctx val)]))
                (id-prompt ctx val))]

           (if-let [link (data/select-here ctx :hf/affix)]
             [hyperfiddle.ui/ui-from-link link ctx props "affix"])

           (if-let [link (data/select-here ctx :hf/remove)]
             [hyperfiddle.ui/ui-from-link link ctx props "remove"])

           (if-let [link (data/select-here ctx :hf/detach)]
             [hyperfiddle.ui/ui-from-link link ctx props "detach"])]))

(defn ^:export id-or-ident [val ctx & [props]]
  ; id control uses links from parent ctx (parent ref and parent path)
  ; select-here does not match :hf/self since it is in the parent ref position
  (if-let [ctx (:hypercrud.browser/parent ctx)]
    [:div
     [:div.input
      ; pr-str here to disambiguate `"tempid"` from `17592186046396` and `:gender/male`
      (or (if-let [self (data/select-here ctx :hf/self)]
            (if val
              [hyperfiddle.ui/ui-from-link self ctx props (pr-str val)]))
          (pr-str val))]

     (if (some-> ctx cardinality (= :db.cardinality/one))
       (if-let [link (data/select-here ctx :hf/affix)]
         [hyperfiddle.ui/ui-from-link link ctx props "affix"]))

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
            doall))]
    [:div [:div.input (pr-str val)]]))

(defn ^:export instant [val ctx & [props]]
  (let [props (-> (assoc props
                    :value val
                    :on-change (with-entity-change! ctx))
                  (update :disabled #(or % (not @(r/track writable-entity? ctx)))))]
    [recom-date props]))

(defn- code-comp [ctx]
  (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
    :hyperfiddle.ui.layout/block contrib.ui/code
    :hyperfiddle.ui.layout/table contrib.ui/code-inline-block))

(defn ^:export code [val ctx & [props]]
  (let [props (-> (assoc props
                    :value val
                    :on-change (with-entity-change! ctx)
                    :parinfer @(runtime/state (:peer ctx) [::runtime/user :hyperfiddle.ide/parinfer]))
                  (update :read-only #(or % (not @(r/track writable-entity? ctx))))
                  (update :mode #(or % "clojure")))]
    [debounced props (code-comp ctx)]))

(defn ^:export css [val ctx & [props]]
  (let [props (-> (assoc props
                    :value val
                    :on-change (with-entity-change! ctx))
                  (update :read-only #(or % (not @(r/track writable-entity? ctx))))
                  (update :mode #(or % "css")))]
    [debounced props (code-comp ctx)]))

(defn ^:export markdown-editor [val ctx & [props]]          ; This is legacy; :mode=markdown should be bound in userland
  (let [props (-> (assoc props
                    :value val
                    :on-change (with-entity-change! ctx)
                    :mode "markdown"
                    :lineWrapping true)
                  (update :read-only #(or % (not @(r/track writable-entity? ctx)))))]
    [debounced props (code-comp ctx)]))

(defn- edn-comp [ctx]
  (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
    :hyperfiddle.ui.layout/block contrib.ui/cm-edn
    :hyperfiddle.ui.layout/table contrib.ui/cm-edn-inline-block))

(defn ^:export edn-many [val ctx & [props]]
  (let [valueType @(context/hydrate-attribute ctx (last (:hypercrud.browser/path ctx)) :db/valueType :db/ident)
        val (set (if (= valueType :db.type/ref) (map (r/partial smart-entity-identifier ctx) val) val))
        props (-> (assoc props
                    :value val
                    :on-change (with-entity-change! ctx))
                  (update :read-only #(or % (not @(r/track writable-entity? ctx)))))]
    [debounced props (edn-comp ctx)]))

(defn ^:export edn [val ctx & [props]]
  (let [props (-> (assoc props
                    :value val
                    :on-change (with-entity-change! ctx))
                  (update :read-only #(or % (not @(r/track writable-entity? ctx)))))]
    [debounced props (edn-comp ctx)]))

(defn ^:export radio-group [val ctx & [props]]
  (into [:span.radio-group (select-keys props [:class])]
        (->> (:options props)
             (map (let [props (dissoc props :class :options)]
                    (fn [option-props]
                      [contrib.ui/radio-with-label
                       (-> (merge props option-props)
                           (update :disabled #(or % (not @(r/track writable-entity? ctx))))
                           (assoc :checked (= (:value option-props) val)
                                  :on-change (with-entity-change! ctx)))]))))))

(defn -magic-new-change! [state ctx #_ov v]
  (let [e @(r/fmap (r/partial smart-entity-identifier ctx)
                   (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))]
    (with-tx! ctx [[:db/add e @state v]])))

(defn magic-new [val ctx props]
  (let [state (r/atom nil)]
    (fn [val ctx props]
      (let [read-only (not @(r/track writable-entity? ctx))]
        [:div
         [contrib.ui/keyword (assoc props
                               :placeholder ":db/ident"
                               :value @state
                               :on-change (r/partial reset! state)
                               :read-only read-only)]
         (let [props (-> (assoc props
                           :magic-new-mode true
                           :on-blur (r/partial -magic-new-change! state ctx)
                           :disabled (let [_ [@state]]      ; force reactions
                                       (or (nil? @state) read-only))
                           :placeholder (pr-str :gender/female)))]
           ; Uncontrolled widget on purpose i think
           ; Cardinality :many not needed, because as soon as we assoc one value, we rehydrate typed
           [contrib.ui/edn props])]))))
