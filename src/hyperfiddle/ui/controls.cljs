(ns hyperfiddle.ui.controls
  (:refer-clojure :exclude [boolean keyword long])
  (:require
    [cats.monad.either :refer [branch]]
    [contrib.css]
    [contrib.data :refer [unqualify]]
    [contrib.pprint :refer [pprint-str]]
    [contrib.reactive :as r]
    [contrib.reader :as reader]
    [contrib.ui :refer [debounced]]                         ; avoid collisions
    [contrib.ui.recom-date :refer [recom-date]]
    [contrib.ui.tooltip :refer [tooltip-thick]]
    [contrib.uri :refer [is-uri?]]
    [datascript.parser :refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]]
    [hypercrud.browser.context :as context]
    [hyperfiddle.data]
    [hyperfiddle.runtime :as runtime]
    #_[hyperfiddle.ui]
    [hyperfiddle.ui.docstring :refer [semantic-docstring]]
    [hyperfiddle.ui.select$ :refer [select]]
    [hyperfiddle.ui.util :refer [with-entity-change! with-tx!]]
    [taoensso.timbre]))


(defn label-with-docs [label help-md props]
  [tooltip-thick (if help-md [:div.hyperfiddle.docstring [contrib.ui/markdown help-md]])
   (let [label-props (select-keys props [:on-click :class])] ; https://github.com/hyperfiddle/hyperfiddle/issues/511
     [:label.hyperfiddle label-props label (if help-md [:sup "†"])])])

(declare hf-new)
(declare hf-remove)

(defn identity-label [_ {:keys [:hypercrud.browser/element] :as ctx} & [props]]
  {:pre [element (:hypercrud.browser/schema ctx) (not (context/qfind-level? ctx))]}
  (let [[_ a _] (context/eav ctx)]
    [:<>
     (label-with-docs (name a) (semantic-docstring ctx) props)
     (if (:hypercrud.browser/head-sentinel ctx)
       ; Tables want hf-new in header
       ; Forms do not, they render it in the cell
       (hf-new _ ctx))
     ; hf/remove handled by body
     #_(hf-remove val ctx)]))

(defn element-label [_ {:keys [:hypercrud.browser/element] :as ctx} & [props]]
  {:pre [(context/qfind-level? ctx) element (:hypercrud.browser/schema ctx)]}
  (let [el @element
        label (case (unqualify (contrib.datomic/parser-type el))
                ; :pull addresses the grouped double header which our grids don't ask for today
                :pull nil
                :variable (:symbol el)
                :aggregate (str (cons (-> el :fn :symbol)
                                      (map (comp second first) (:args el)))))]
    (label-with-docs label (semantic-docstring ctx) props)))

(defn ref-label [_ ctx & [props]]
  (let [[_ a _] (context/eav ctx)
        label (name a)]
    ; top-level ref should get it though?
    [:<>
     (label-with-docs label (semantic-docstring ctx) props)
     #_(if (= 1 (context/pull-depth ctx))
       ; :community/neighborhood gets new-neighborhood? NO
       (hf-new _ ctx))]))

(defn id-prompt [ctx val]
  ; pr-str here to disambiguate `"tempid"` from `17592186046396` and `:gender/male`
  ; ... This is dumb datomic hacks, views should never even see tempids
  (let [[_ _ v] (context/eav ctx)]
    (if v
      (str v)
      "nil")))

(defn hf-new [_ ctx]
  (for [[k r-link] (hyperfiddle.data/spread-links-here ctx :hf/new)]
    ^{:key k}
    [hyperfiddle.ui/ui-from-link r-link ctx]))

(defn hf-remove [val ctx]
  ; (if-not (:hypercrud.browser/head-sentinel ctx))
  (if val
    (for [[k r-link] (hyperfiddle.data/spread-links-here ctx :hf/remove)]
      ^{:key k}
      [hyperfiddle.ui/ui-from-link r-link ctx])))

(defn related-links "Refs don't call this directly, but scalars can.
  These are like Right JOIN. If there is no ref you can't traverse it."
  [val ctx]
  (if val
    (doall
      ; or just here? http://tank.hyperfiddle.site/:dustingetz!counter/
      (for [[k rv] (hyperfiddle.data/spread-links-here ctx)
            :when (not (some #{:hf/new :hf/remove :hf/iframe}
                             (:link/class @rv)))]
        [k rv]))))

(defn render-related-links [val ctx]
  [:<>
   (->> (for [[k rv] (related-links val ctx)]
          ^{:key k}
          [hyperfiddle.ui/ui-from-link rv ctx])
        (interpose "·")
        doall)])

(defn entity-links [val ctx &  more-links]
  (let [[[k rv] :as rkvs] (related-links val ctx)]
    (cond
      (= 1 (count rkvs))
      ; Use the id-prompt as the anchor label if there is only one related link.
      ; There is almost always only one. This conserves a lot of width.
      [:<>
       [:div.input [hyperfiddle.ui/ui-from-link rv ctx nil (id-prompt ctx val)]]
       more-links]

      :else
      ; Disambiguate the links with link labels
      [:<>
       [:div.input (id-prompt ctx val)]
       (-> (for [[k rv] rkvs]
             ^{:key k}
             [hyperfiddle.ui/ui-from-link rv ctx])
           (concat more-links)
           (->> (interpose "·"))
           doall)])))

(defn ^:export ref [val ctx & [props]]
  (cond
    (:options props)
    [select val ctx props]

    :else
    [:div.hyperfiddle-input-group
     (entity-links val ctx
                   (hf-new val ctx)                         ; new child
                   (hf-remove val ctx))]))

(defn ^:export keyword [val ctx & [props]]
  [:div.hyperfiddle-input-group
   (let [props (assoc props :value val :on-change (with-entity-change! ctx))]
     [debounced props contrib.ui/keyword])
   (hf-remove val ctx)                                      ; why? They can just backspace it
   (render-related-links val ctx)])

(defn ^:export ref-many [val ctx & [props]]
  [hyperfiddle.ui/table hyperfiddle.ui/columns ctx props])

(defn ^:export identity-control [val ctx & [props]]
  [:div.hyperfiddle-input-group
   ; hf/new can't just be slammed in, because once semantic, it will be available
   ; in places where it wasn't asked for. So in datamode, what do you do?
   ; I think we ignore it in forms, and only draw it in table headers.
   (entity-links val ctx
                 ; http://tank.hyperfiddle.site/:dustingetz!gender-shirtsize/
                 ; http://alexandrkozyrev.hyperfiddle.site/:codeq/
                 #_(hf-new val ctx) ; in table context, only a ref if this attr is ref.
                 (if-not (context/underlying-tempid ctx (context/e ctx)) ; val can be part of lookup ref scalar
                   (hf-remove val ctx)))])

(defn ^:export instant [val ctx & [props]]
  (let [props (assoc props :value val :on-change (with-entity-change! ctx))]
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
                  (update :mode #(or % "clojure")))]
    [debounced props (code-comp ctx)]))

(defn ^:export css [val ctx & [props]]
  (let [props (-> (assoc props
                    :value val
                    :on-change (with-entity-change! ctx))
                  (update :mode #(or % "css")))]
    [debounced props (code-comp ctx)]))

(defn ^:export markdown-editor [val ctx & [props]]          ; This is legacy; :mode=markdown should be bound in userland
  (let [props (-> (assoc props
                    :value val
                    :on-change (with-entity-change! ctx)
                    :mode "markdown"
                    :lineWrapping true))]
    [debounced props (code-comp ctx)]))

(defn value-validator [ctx]
  (let [[_ a _] @(:hypercrud.browser/eav ctx)]
    (case (contrib.datomic/valueType @(:hypercrud.browser/schema ctx) a)
      :db.type/bigdec any?                                  ;  todo
      :db.type/bigint any?                                  ;  todo
      :db.type/boolean boolean?
      :db.type/bytes any?                                   ;  todo
      :db.type/double number?
      :db.type/float number?
      :db.type/fn any?                                      ;  todo
      :db.type/instant any?                                 ;  todo
      :db.type/keyword keyword?
      :db.type/long any?                                    ;  todo
      :db.type/ref any?                                     ;  todo
      :db.type/string string?
      :db.type/uri is-uri?
      :db.type/uuid uuid?
      nil (do
            (taoensso.timbre/warn "unknown valueType: " a)  ; :domain/_databases
            (constantly true)))))

(let [parse-string (fn [value-pred s]
                     (let [v (reader/read-edn-string! s)]
                       (assert (every? value-pred v))
                       v))]
  (defn ^:export edn-many [val ctx & [props]]
    [:div.hyperfiddle-input-group
     ; Links aren't handled, we need to isolate individual values for that
     (let [[_ a _] @(:hypercrud.browser/eav ctx)
           val (set (if (contrib.datomic/valueType? @(:hypercrud.browser/schema ctx) a :db.type/ref)
                      (map (r/partial context/smart-entity-identifier ctx) val)
                      val))
           props (-> (assoc props
                       :value val
                       :mode "clojure"
                       :on-change (with-entity-change! ctx)))]
       [debounced props contrib.ui/validated-cmp (r/partial parse-string (value-validator ctx)) pprint-str
        (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
          :hyperfiddle.ui.layout/block contrib.ui/code
          :hyperfiddle.ui.layout/table contrib.ui/code-inline-block)])]))

(let [parse-string (fn [value-pred s]
                     (let [v (reader/read-edn-string! s)]
                       (assert ((some-fn nil? value-pred) v))
                       v))]
  (defn ^:export edn [val ctx & [props]]
    [:div.hyperfiddle-input-group
     (let [props (assoc props
                   :value val
                   :mode "clojure"
                   :on-change (with-entity-change! ctx))]
       [debounced props contrib.ui/validated-cmp
        (r/partial parse-string (value-validator ctx))
        pprint-str
        (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
          :hyperfiddle.ui.layout/block contrib.ui/code
          :hyperfiddle.ui.layout/table contrib.ui/code-inline-block)])
     (render-related-links val ctx)]))

(defn ^:export radio-group [val ctx & [props]]
  (into [:span.radio-group (-> (select-keys props [:class])
                               (update :class contrib.css/css (when (:is-invalid props) "invalid")))]
        (->> (:options props)
             (map (let [props (dissoc props :is-invalid :class :options)]
                    (fn [option-props]
                      [contrib.ui/radio-with-label
                       (-> (merge props option-props)
                           (assoc :checked (or (= (:value option-props) val)
                                               (and (nil? val) (= (:value option-props) (:default-value props))))
                                  :on-change (with-entity-change! ctx)))]))))))

(defn -magic-new-change! [state ctx #_ov v]
  (let [[e _ _] @(:hypercrud.browser/eav ctx)]
    (assert e)
    (with-tx! ctx [[:db/add e @state v]])))

(defn magic-new [val ctx props]
  (let [state (r/atom nil)]
    (fn [val ctx props]
      [:div
       [contrib.ui/keyword (assoc props
                             :placeholder ":db/ident"
                             :value @state
                             :on-change (r/partial reset! state)
                             :read-only (:disabled props))]
       (let [props (-> (assoc props
                         :magic-new-mode true
                         :on-blur (r/partial -magic-new-change! state ctx)
                         :disabled (let [_ [@state]]        ; force reactions
                                     (or (nil? @state) (:disabled props)))
                         :placeholder (pr-str :gender/female)))]
         ; Uncontrolled widget on purpose i think
         ; Cardinality :many not needed, because as soon as we assoc one value, we rehydrate typed
         [contrib.ui/edn props])])))

(defn ^:export string [val ctx & [props]]
  [:div.hyperfiddle-input-group
   (let [props (assoc props :value val :on-change (with-entity-change! ctx))]
     [debounced props contrib.ui/text #_contrib.ui/textarea])
   (render-related-links val ctx)])

(defn ^:export long [val ctx & [props]]
  [:div.hyperfiddle-input-group
   (let [props (assoc props :value val :on-change (with-entity-change! ctx))]
     [debounced props contrib.ui/long])
   (render-related-links val ctx)])

(defn ^:export boolean [val ctx & [props]]
  [:div.hyperfiddle-input-group
   [:div (select-keys props [:class :style])
    (let [props (assoc props
                  :checked (clojure.core/boolean val)
                  :on-change (with-entity-change! ctx))]
      [contrib.ui/easy-checkbox
       (select-keys props [:class :style :is-invalid :checked :on-change :disabled])])] ; readonly?
   (render-related-links val ctx)])

(let [adapter (fn [e]
                (case (.-target.value e)
                  "" nil
                  "true" true
                  "false" false))]
  (defn ^:export tristate-boolean [val ctx & [props]]
    [:div.hyperfiddle-input-group
     (let [option-props (select-keys props [])
           select-props (select-keys props [:value :on-change :class :style :disabled])]
       [:select (assoc select-props
                  :value (if (nil? val) "" (str val))
                  :on-change (r/comp (with-entity-change! ctx) adapter))
        [:option (assoc option-props :key true :value "true") "True"]
        [:option (assoc option-props :key false :value "false") "False"]
        [:option (assoc option-props :key :nil :value "") "--"]])
     (render-related-links val ctx)]))
