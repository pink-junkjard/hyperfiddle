(ns hyperfiddle.ui
  (:require-macros [hyperfiddle.ui :refer [-build-fiddle]])
  (:require
    [cats.core :as cats :refer [fmap >>=]]
    [cats.monad.either :as either]
    [clojure.core.match :refer [match match*]]
    [clojure.string :as string]
    [contrib.ct :refer [unwrap]]
    [contrib.css :refer [css css-slugify]]
    [contrib.eval :as eval]
    [contrib.pprint :refer [pprint-str]]
    [contrib.reactive :as r]
    [contrib.string :refer [blank->nil]]
    [contrib.ui]
    [contrib.ui.safe-render :refer [user-portal]]
    [contrib.ui.tooltip :refer [tooltip tooltip-props]]
    [contrib.validation]
    [datascript.parser :refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.routing :as routing]
    [hypercrud.ui.connection-color :refer [connection-color]]
    [hypercrud.ui.error :as ui-error]
    [hypercrud.ui.stale :as stale]
    [hyperfiddle.data :as data]
    [hyperfiddle.domain]
    [hyperfiddle.ide.console-links]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime]
    [hyperfiddle.security.client :as security]
    [hyperfiddle.ui.api]
    [hyperfiddle.ui.controls :as controls :refer [label-with-docs dbid-label magic-new]]
    [hyperfiddle.ui.docstring :refer [semantic-docstring]]
    [hyperfiddle.ui.iframe :refer [iframe-cmp]]
    [hyperfiddle.ui.popover :refer [effect-cmp popover-cmp]]
    [hyperfiddle.ui.select$]
    [hyperfiddle.ui.sort :as sort]
    [hyperfiddle.ui.util :refer [writable-entity?]]))


(let [memoized-eval-expr-str!+ (memoize eval/eval-expr-str!+) ; don't actually need to safely eval, just want to memoize exceptions)
      ; defer eval until render cycle inside userportal
      eval-renderer-comp (fn [fiddle-renderer-str & args]
                           (either/branch
                             (memoized-eval-expr-str!+ fiddle-renderer-str)
                             (fn [e] (throw e))
                             (fn [f] (into [f] args))))]
  (defn attr-renderer-control [val ctx & [props]]
    ; The only way to stabilize this is for this type signature to become a react class.
    (when-let [user-f (-> @(r/cursor (:hypercrud.browser/attr-renderers ctx) [(last (:hypercrud.browser/path ctx))])
                          blank->nil)]
      [user-portal (ui-error/error-comp ctx) nil
       ; ?user-f is stable due to memoizing eval (and only due to this)
       [eval-renderer-comp user-f val ctx props]])))

(declare result)
(declare pull)
(declare field)
(declare hyper-control)
(declare link)
(declare ui-from-link)
(declare fiddle-api)
(declare fiddle-xray)

#_(defn entity-links-iframe [val ctx & [props]]
  ; Find the iframe links that we can get to downwards, but not upwards
  ; (select searches upwards)
  ; Only ask this question from a place at the top
  (->> (r/fmap->> (data/select-all-r ctx :hf/iframe)
                  (remove (r/comp (r/partial context/deps-over-satisfied? ctx) context/read-path :link/path)))
       (r/unsequence (r/partial context/stable-relation-key ctx))
       (map (fn [[rv k]]
              ^{:key k}
              [ui-from-link rv ctx props]))
       (into [:<>])))

(defn iframe-field-default [val ctx props]
  (let [props (-> props (dissoc :class) (assoc :label-fn (r/constantly nil) #_[:div "nested pull iframes"]))]
    [:pre "iframe-field-default"]
    #_[field [] ctx entity-links-iframe props]))

(defn control "this is a function, which returns component"
  [val ctx & [props]]                                       ; returns Func[(ref, props, ctx) => DOM]
  (let [segment (last (:hypercrud.browser/path ctx))
        attr @(context/hydrate-attribute ctx segment)
        type (or (some-> attr :db/valueType :db/ident name keyword) (context/segment-type-2 segment)) ; can include :element ? :aggregate, :entity
        cardinality (some-> attr :db/cardinality :db/ident name keyword)]
    (match* [type cardinality]
      [:element _] controls/string                          ; FindRel-Variable
      #_#_[:aggregate _] controls/string                    ; entity, aggregate, what else?
      #_#_[:entity _] entity-links
      [:attribute _] (r/constantly (str "no schema for attr: " segment))

      [:splat _] magic-new
      [:boolean :one] controls/boolean
      [:keyword :one] controls/keyword
      [:string :one] controls/string
      [:long :one] controls/long
      [:instant :one] controls/instant
      [:ref :one] controls/ref
      [:ref :many] controls/ref-many
      [_ :one] controls/edn
      [_ :many] controls/edn-many)))

(defn ^:export hyper-control [val ctx & [props]]
  {:post [%]}
  ;(js/console.warn (str (:db/ident @(context/hydrate-attribute ctx (last (:hypercrud.browser/path ctx))))))
  (or (attr-renderer-control val ctx props)
      (let [-field @(:hypercrud.browser/field ctx)]
        (cond                                               ; Duplicate options test to avoid circular dependency in controls/ref
          (:options props) [(control val ctx props) val ctx props]
          (field/identity-segment? -field) [controls/id-or-ident val ctx props]
          (field/children-identity-only? -field) [(control val ctx props) val ctx props]
          (seq (::field/children -field)) (let [ctx (dissoc ctx ::layout)]
                                            [:div           ; wrapper div: https://github.com/hyperfiddle/hyperfiddle/issues/541
                                             [pull field val ctx props]
                                             [iframe-field-default val ctx props]])
          :else [(control val ctx props) val ctx props]))))

(defn ^:export hyper-label [_ ctx & [props]]
  (let [field @(:hypercrud.browser/field ctx)
        level (::field/level field)
        segment (last (:hypercrud.browser/path ctx))
        segment-type (context/segment-type-2 segment)       ; :element means :relation? no, naked. Relation is ortho
        has-child-fields @(r/fmap-> (:hypercrud.browser/field ctx) ::field/children nil? not)]
    (cond
      (field/identity-segment? field) (dbid-label _ ctx props)
      :else (match* [level segment-type segment has-child-fields]
              [nil :splat _ _] (label-with-docs (::field/label field) (semantic-docstring ctx "Free-hand attribute entry") props)
              [nil :attribute _ _] (label-with-docs (::field/label field) (semantic-docstring ctx) props)
              [nil :element _ true] (label-with-docs "*relation*" (semantic-docstring ctx) props)
              [nil :element _ false] (label-with-docs (::field/label field) (semantic-docstring ctx) props)
              [nil :naked-or-element _ _] (label-with-docs "*relation*" (semantic-docstring ctx) props) ; Schema new attr, and fiddle-links new link - needs to be split
              [:relation :naked-or-element _ _] (label-with-docs "*relation*" (semantic-docstring ctx) props)
              [:tuple :naked-or-element _ _] (label-with-docs "*tuple*" (semantic-docstring ctx) props)))))


(defn ^:export semantic-css [ctx]
  ; Include the fiddle level ident css.
  ; Semantic css needs to be prefixed with - to avoid collisions. todo
  (->> (concat
         ["hyperfiddle"
          (context/dbname ctx)                              ; color
          (name (context/segment-type-2 (last (:hypercrud.browser/path ctx))))
          (string/join "/" (:hypercrud.browser/path ctx))   ; legacy unique selector for each location
          (->> (:hypercrud.browser/path ctx)                ; actually generate a unique selector for each location
               (cons :hypercrud.browser/path)               ; need to prefix the path with something to differentiate between attr and single attr paths
               (string/join "/"))]
         (when (context/attribute-segment? (last (:hypercrud.browser/path ctx)))
           (let [attr-ident (last (:hypercrud.browser/path ctx))]
             [@(context/hydrate-attribute ctx attr-ident :db/valueType :db/ident)
              @(context/hydrate-attribute ctx attr-ident :db/cardinality :db/ident)
              (some-> @(context/hydrate-attribute ctx attr-ident :db/isComponent) (if :component))]))
         (:hypercrud.browser/path ctx))
       (map css-slugify)
       (apply css)))

(defn ^:export value "Relation level value renderer. Works in forms and lists but not tables (which need head/body structure).
User renderers should not be exposed to the reaction."
  ; Path should be optional, for disambiguation only. Naked is an error
  [relative-path ctx ?f & [props]]                          ; ?f :: (ref, props, ctx) => DOM
  (let [ctx (context/focus ctx relative-path)
        props (update props :class css (semantic-css ctx))]
    [(or ?f hyper-control) @(:hypercrud.browser/data ctx) ctx props]))

(defn ^:export anchor [ctx props & children]
  (let [props (-> props
                  (update :class css "hyperfiddle")
                  (dissoc :route)
                  (assoc :href (some->> (:route props) (hyperfiddle.foundation/route-encode (:peer ctx)))))]
    (into [:a props] children)))

(letfn [(prompt [link-ref ?label]
          (or ?label
              (->> (set @(r/fmap :link/class link-ref)) (map name) (interpose " ") (apply str) blank->nil)
              (some-> @link-ref :link/fiddle :fiddle/ident name)
              (some-> @link-ref :link/tx-fn name)))
        (link-tooltip [{:link/keys [class path]} ?route ctx] ; Show how it routed. The rest is obvious from data mode
          (if-let [[fiddle-ident args] ?route]
            (->> (concat #_class #_[fiddle-ident] args)
                 (map pr-str) (interpose " ") (apply str))))
        (validated-route-tooltip-props [r+?route link-ref ctx props] ; link is for validation
          ; this is a fine place to eval, put error message in the tooltip prop
          ; each prop might have special rules about his default, for example :visible is default true, does this get handled here?
          (let [+route (>>= @r+?route #(routing/validated-route+ (:link/fiddle @link-ref) % ctx))
                errors (->> [+route] (filter either/left?) (map cats/extract) (into #{}))
                ?route (unwrap (constantly nil) +route)]
            (-> props
                (assoc :route ?route)
                (update :tooltip (fn [existing-tooltip]
                                   (if-not (empty? errors)
                                     [:warning (pprint-str errors)]
                                     (if (:hyperfiddle.ui/debug-tooltips ctx)
                                       [nil (link-tooltip @link-ref ?route ctx)]
                                       existing-tooltip))))
                (update :class css (when-not (empty? errors) "invalid")))))
        (disabled? [link-ref ctx]
          (condp some @(r/fmap :link/class link-ref)
            #{:hf/new} nil #_(not @(r/track security/can-create? ctx)) ; flag
            #{:hf/remove} (if @(r/cursor (:hypercrud.browser/eav ctx) [1])
                            (not @(r/track security/writable-entity? (:hypercrud.browser/parent ctx)))
                            (not @(r/track security/writable-entity? ctx)))
            ; else we don't know the semantics, just nil out
            nil))]
  (defn ui-from-link [link-ref ctx & [props ?label]]
    {:pre [link-ref (r/reactive? link-ref)]}
    (let [visual-ctx ctx
          [ctx r+?route] (context/refocus' ctx link-ref)    ; route reaction isn't even real
          style {:color nil #_(connection-color ctx (cond (hyperfiddle.ide.console-links/system-link? (:db/id @link-ref)) 60 :else 40))}
          props (update props :style #(or % style))
          has-tx-fn @(r/fmap-> link-ref :link/tx-fn blank->nil boolean)
          is-iframe @(r/fmap->> link-ref :link/class (some #{:hf/iframe}))]
      (cond
        (and has-tx-fn @(r/fmap-> link-ref :link/fiddle nil?))
        (let [props (-> props
                        (update :class css "hyperfiddle")
                        (update :disabled #(or % (disabled? link-ref ctx))))]
          [effect-cmp link-ref ctx props @(r/track prompt link-ref ?label)])

        (or has-tx-fn (and is-iframe (:iframe-as-popover props)))
        (let [props (-> @(r/track validated-route-tooltip-props r+?route link-ref ctx props)
                        (dissoc :iframe-as-popover)
                        (update :class css "hyperfiddle")   ; should this be in popover-cmp? what is this class? – unify with semantic css
                        (update :disabled #(or % (disabled? link-ref ctx))))
              label @(r/track prompt link-ref ?label)]
          [popover-cmp link-ref ctx visual-ctx props label])

        is-iframe
        [stale/loading (stale/can-be-loading? ctx) (fmap #(route/assoc-frag % (:frag props)) @r+?route) ; what is this frag noise?
         (fn [e] [(ui-error/error-comp ctx) e])
         (fn [route]
           (let [iframe (or (::custom-iframe props) iframe-cmp)]
             [iframe ctx (-> props                          ; flagged - :class
                             (assoc :route route)
                             (dissoc props ::custom-iframe)
                             (update :class css (->> @(r/fmap :link/class link-ref)
                                                     (string/join " ")
                                                     css-slugify)))]))]

        :else (let [props @(r/track validated-route-tooltip-props r+?route link-ref ctx props)]
                [tooltip (tooltip-props (:tooltip props))
                 (let [props (dissoc props :tooltip)]
                   ; what about class - flagged
                   ; No eav, the route is the same info
                   [anchor ctx props @(r/track prompt link-ref ?label)])])
        ))))

(defn ^:export link "Relation level link renderer. Works in forms and lists but not tables." ; this is dumb, use a field renderer
  [corcs ctx & [?label props]]                              ; path should be optional, for disambiguation only. Naked can be hard-linked in markdown?
  (either/branch
    (data/select+ ctx corcs)
    #(vector :span %)
    (fn [link-ref]
      [ui-from-link link-ref ctx props ?label])))

(defn ^:export browse "Relation level browse. Works in forms and lists but not tables."
  [corcs ctx & [?user-renderer props]]
  {:pre [ctx]}
  (let [props (if ?user-renderer
                (assoc props :user-renderer ?user-renderer)
                props)]
    (either/branch
      (data/select+ ctx corcs)
      #(vector :span %)
      (fn [link-ref]
        [ui-from-link link-ref ctx props]))))

(defn form-field "Form fields are label AND value. Table fields are label OR value."
  [relative-path ctx Body Head props]
  (with-meta
    [:div {:class (css "field" (:class props))
           :style {:border-color (connection-color ctx)}}
     [Head nil (dissoc ctx :hypercrud.browser/data) props]  ; eav
     (let [[e a v] @(:hypercrud.browser/eav ctx)
           props (as-> props props
                       (update props :disabled #(or % (not @(r/track writable-entity? ctx))))
                       (update props :is-invalid #(or % (context/leaf-invalid? ctx)))
                       (update props :class css (if (:disabled props) "disabled")))]
       [Body v ctx props])]
    (when (= '* (last relative-path))                       ; :hypercrud.browser/path
      ; guard against crashes for nil data
      {:key @(r/fmap-> (or (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])
                           (r/track identity nil))
                       keys hash)})))

(defn table-field "Form fields are label AND value. Table fields are label OR value."
  [relative-path ctx Body Head props]                       ; Body :: (val props ctx) => DOM, invoked as component
  ; Presence of data to detect head vs body? Kind of dumb
  (case (if (:hypercrud.browser/data ctx) :body :head)      ; this is ugly and not uniform with form-field NOTE: NO DEREF ON THIS NIL CHECK
    :head [:th {:class (css "field" (:class props))         ; hoist
                :style {:background-color (connection-color ctx)}}
           [Head nil ctx (-> props
                             (update :class css (when (sort/sortable? ctx) "sortable") (some-> (sort/sort-direction relative-path ctx) name))
                             (assoc :on-click (r/partial sort/toggle-sort! relative-path ctx)))]]
    ; Field omits [] but table does not, because we use it to specifically draw repeating anchors with a field renderer.
    :body [:td {:class (css "field" (:class props))
                :style {:border-color (connection-color ctx)}}
           (let [props (as-> props props
                             (update props :disabled #(or % (not @(r/track writable-entity? ctx))))
                             (update props :is-invalid #(or % (context/leaf-invalid? ctx)))
                             (update props :class css (if (:disabled props) "disabled")))]
             [Body @(:hypercrud.browser/data ctx) ctx props])]))

(defn ^:export field "Works in a form or table context. Draws label and/or value."
  [relative-path ctx & [?f props]]
  (let [ctx (context/focus ctx relative-path)
        Body (or ?f hyper-control)
        Head (or (:label-fn props) hyper-label)
        props (dissoc props :label-fn)
        props (update props :class css (semantic-css ctx))
        is-magic-new (= '* (last relative-path))]
    (case (:hyperfiddle.ui/layout ctx)                      ; check cardinality
      :hyperfiddle.ui.layout/table (when-not is-magic-new
                                     [table-field relative-path ctx Body Head props])
      [form-field relative-path ctx Body Head props])))

(defn ^:export table "Semantic table; columns driven externally" ; this is just a widget
  [columns ctx & [props]]
  (let [sort-col (r/atom (::sort/initial-sort props))]
    (fn [columns ctx & [props]]
      (let [props (dissoc props ::sort/initial-sort)
            ctx (assoc ctx ::sort/sort-col sort-col
                           ::layout :hyperfiddle.ui.layout/table)]
        [:table (update props :class (fnil css "hyperfiddle") "unp") ; fnil case is iframe root (not a field :many)
         [:thead (->> (columns (dissoc ctx :hypercrud.browser/data) props) (into [:tr]))] ; strict
         ; filter? Group-by? You can't. Do it in the peer
         (->> (for [ctx (hyperfiddle.api/spread-rows ctx #(sort/sort-fn % sort-col))]
                (->> (columns ctx props)
                     (into [:tr {:key (:hypercrud.browser/path ctx)}])))
              (into [:tbody]))]))))

(defn hint [val {:keys [hypercrud.browser/fiddle] :as ctx} props]
  (if (and (-> (:fiddle/type @fiddle) (= :entity))
           (empty? val))
    [:div.alert.alert-warning "Warning: invalid route (d/pull requires an entity argument). To add a tempid entity to the URL, click here: "
     [:a {:href "~entity('$','tempid')"} [:code "~entity('$','tempid')"]] "."]))

(defn form "Not an abstraction." [fields val ctx & [props]]
  (into [:<> {:key (str (context/row-keyfn ctx val))}]
        (fields (assoc ctx ::layout :hyperfiddle.ui.layout/block))))

(defn columns [m-field relative-path field ctx & [props]]
  (for [ctx (hyperfiddle.api/spread-relation ctx)]          ; (hyperfiddle.api/spread-a ctx)
    [form-field ctx Body Head props])
  (concat
    (->> @(r/fmap->> m-field ::field/children (map ::field/path-segment))
         (map (fn [child-segment]
                (let [relative-path (conj relative-path child-segment)]
                  ^{:key (str relative-path)}
                  [field relative-path ctx hyper-control props]))))
    (when-let [f (condp = @(r/fmap ::field/element-type m-field)
                   Variable hyper-control
                   Aggregate hyper-control
                   Pull nil #_entity-links
                   ; else nested pulls
                   nil)]
      [^{:key (str relative-path)} [field relative-path ctx f props]]))) ; fiddle segment (top)

(defn columns-relation-product [field ctx & [props]]
  (->> (r/fmap ::field/children (:hypercrud.browser/field ctx))
       (r/unsequence ::field/path-segment)
       (mapcat (fn [[m-field segment]]
                 (columns m-field [segment] field ctx props)))))

(defn pull "handles any datomic result that isn't a relation, recursively"
  [field val ctx & [props]]
  (let [cardinality @(r/fmap ::field/cardinality (:hypercrud.browser/field ctx))]
    (match* [cardinality]
      [:db.cardinality/one] [form (r/partial columns (:hypercrud.browser/field ctx) [] field) val ctx props]
      [:db.cardinality/many] [table (r/partial columns (:hypercrud.browser/field ctx) [] field) ctx props])))

(defn ^:export result "Default result renderer. Invoked as fn, returns seq-hiccup, hiccup or
nil. call site must wrap with a Reagent component"          ; is this just hyper-control ?
  [val ctx & [props]]
  [:<>
   (hint val ctx props)
   (let [type @(r/fmap :fiddle/type (:hypercrud.browser/fiddle ctx))
         level @(r/fmap ::field/level (:hypercrud.browser/field ctx))]
     (match* [type level]
       [:blank _] nil
       [:query :relation] [table (r/partial columns-relation-product field) ctx props]
       [:query :tuple] [form (r/partial columns-relation-product field) val ctx props]
       [_ _] (pull field val ctx props)))
   [iframe-field-default val ctx props]])

(def ^:dynamic markdown)                                    ; this should be hf-contrib or something

(def ^:export fiddle (-build-fiddle))

(defn ^:export fiddle-xray [val ctx & [props]]
  [:div (select-keys props [:class :on-click])
   [:h3 (pr-str @(:hypercrud.browser/route ctx))]
   (for [ctx (hyperfiddle.api/spread-fiddle ctx)]
     [result val ctx {}])])

(letfn [(render-edn [data]
          (let [edn-str (pprint-str data 160)]
            [contrib.ui/code {:value edn-str :read-only true}]))]
  (defn ^:export fiddle-api [val ctx & [props]]
    (let [data (hyperfiddle.ui.api/api-data ctx)]
      [:div.hyperfiddle.display-mode-api.container-fluid (select-keys props [:class])
       [:h3
        [:dl
         [:dt "route"] [:dd (pr-str @(:hypercrud.browser/route ctx))]]]
       (render-edn (get data @(:hypercrud.browser/route ctx)))
       (->> (dissoc data @(:hypercrud.browser/route ctx))
            (map (fn [[route result]]
                   ^{:key (str (hash route))}
                   [:div
                    [:dl [:dt "route"] [:dd (pr-str route)]]
                    (render-edn result)])))])))

(defn ^:export img [val ctx & [props]]
  [:img (merge props {:src val})])
