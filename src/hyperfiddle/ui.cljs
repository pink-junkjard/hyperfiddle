(ns hyperfiddle.ui
  (:require-macros [hyperfiddle.ui :refer [-build-fiddle]])
  (:require
    [cats.core :as cats :refer [fmap >>=]]
    [cats.monad.either :as either :refer [branch]]
    [cljs.spec.alpha :as s]
    [clojure.core.match :refer [match match*]]
    [clojure.string :as string]
    [contrib.ct :refer [unwrap]]
    [contrib.css :refer [css css-slugify]]
    [contrib.data :refer [unqualify]]
    [contrib.eval :as eval]
    [contrib.pprint :refer [pprint-str]]
    [contrib.reactive :as r]
    [contrib.string :refer [blank->nil]]
    [contrib.ui]
    [contrib.ui.safe-render :refer [user-portal]]
    [contrib.ui.tooltip :refer [tooltip tooltip-props]]
    [contrib.validation]
    [datascript.parser :refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.routing :as routing]
    [hypercrud.ui.error :as ui-error]
    [hypercrud.ui.stale :as stale]
    [hyperfiddle.api :as hf]
    [hyperfiddle.data :as data]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security.client :as security]
    [hyperfiddle.ui.controls :as controls :refer [label-with-docs identity-label ref-label element-label magic-new]]
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
    (when-let [user-f (->> (second (context/eav ctx))
                           (runtime/attribute-renderer (:peer ctx) (:branch ctx))
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

(defn entity-links-iframe [val ctx & [props]]
  ; Remove links that are also available at our ancestors.
  ; http://hyperfiddle.hyperfiddle.site/:hyperfiddle.ide!domain/~entity('$domains',(:domain!ident,'hyperfiddle'))
  ; http://tank.hyperfiddle.site/:tutorial.race!submission/~entity('$',(:dustingetz.reg!email,'bob@example.com'))
  ; http://tank.hyperfiddle.site/:dustingetz.seattle!communities/
  (if (or (= (context/depth ctx) 0)
          (and (> (context/depth ctx) 0)
               (let [[_ a _] (context/eav ctx)]
                 (not (contrib.datomic/ref-one? @(:hypercrud.browser/schema ctx) a)))))
    [:<>
     ; keep flip flopping. is it here, or in dimension ??
     (for [[k r-link] (hyperfiddle.data/spread-links-in-dimension ctx :hf/iframe)]
       ^{:key k}
       [ui-from-link r-link ctx props])]))

(defn iframe-field-default [val ctx props]
  (let [props (-> props (dissoc :class) (assoc :label-fn (r/constantly nil) #_[:div "nested pull iframes"]))]
    [field [] ctx entity-links-iframe props]))

(defn render-ref [ctx props]
  (let [children (context/children ctx)]
    (cond
      (:options props) [hyperfiddle.ui.select$/select nil ctx props] ; legacy, use attr renderers for this

      (and (seq children)
           (= 1 (count children))
           (every? (partial context/identity? ctx) children))
      ; This if statement is needed to flatten test case: http://hyperfiddle.hyperfiddle.site/:databases/
      ; Dude i have no idea why this if statement is necessary
      (if (context/attr? ctx :db.cardinality/many)
        [controls/ref-many (context/data ctx) ctx props]
        (let [[a] children                                  ; pick one, best if there is only one
              ctx (context/focus ctx [a])]
          (hf/render ctx props)))

      (seq children)
      (let [ctx (dissoc ctx ::layout)]
        [:div                                               ; wrapper div: https://github.com/hyperfiddle/hyperfiddle/issues/541
         [pull (context/data ctx) ctx props]                               ; account for hf/new at parent ref e.g. :community/neighborhood
         #_[iframe-field-default (context/data ctx) ctx props]])

      (context/attr? ctx :db.cardinality/one)
      [controls/ref (context/data ctx) ctx props]

      (context/attr? ctx :db.cardinality/many)
      [controls/ref-many (context/data ctx) ctx props])))

(defmethod hf/render #{:db.type/ref :db.cardinality/one} [ctx props] (render-ref ctx props))
(defmethod hf/render #{:db.type/ref :db.cardinality/many} [ctx props] (render-ref ctx props))

(defmethod hf/render #{:hf/variable} [ctx props]
  [controls/string (context/data ctx) ctx props])

(defmethod hf/render #{:hf/aggregate} [ctx props]
  [controls/string (context/data ctx) ctx props])

(defmethod hf/render #{:db.unique/identity} [ctx props]
  [controls/identity-control (context/data ctx) ctx props])

(defmethod hf/render #{:db.type/boolean :db.cardinality/one} [ctx props]
  [controls/boolean (context/data ctx) ctx props])

(defmethod hf/render #{:db.type/keyword :db.cardinality/one} [ctx props]
  [controls/keyword (context/data ctx) ctx props])

(defmethod hf/render #{:db.type/string :db.cardinality/one} [ctx props]
  [controls/string (context/data ctx) ctx props])

(defmethod hf/render #{:db.type/long :db.cardinality/one} [ctx props]
  [controls/long (context/data ctx) ctx props])

(defmethod hf/render #{:db.type/instant :db.cardinality/one} [ctx props]
  [controls/instant (context/data ctx) ctx props])

(defmethod hf/render :default [ctx props]
  (cond
    (context/attr? ctx :db.cardinality/one)
    [controls/edn (context/data ctx) ctx props]

    (context/attr? ctx :db.cardinality/many)
    [controls/edn-many (context/data ctx) ctx props]

    :else
    [:pre (pr-str "render: no match for eav: " (context/eav ctx))]))

(defn ^:export hyper-control "Val is for userland field renderers, our builtin controls use ctx and ignore val."
  [val ctx & [props]]
  {:post [%]}
  (or (attr-renderer-control val ctx props)               ; compat
      (hf/render ctx props)))

(defn ^:export hyper-label [_ ctx & [props]]                ; props has sort :on-click
  (let [?element (:hypercrud.browser/element ctx)
        element-type (if ?element (contrib.datomic/parser-type @?element))
        i (:hypercrud.browser/element-index ctx)
        [_ a _] (context/eav ctx)
        attr (and a (context/attr ctx a))]
    (match* [i (unqualify element-type) a attr]             ; has-child-fields @(r/fmap-> (:hypercrud.browser/field ctx) ::field/children nil? not)
      [_ nil _ _] nil                                       ; e.g. !field[js/user.hyperblog-post-link]()
      [_ :pull :db/id _] (identity-label _ ctx props)
      [_ :pull :db/ident _] (identity-label _ ctx props)
      [_ :pull _ {:db/unique :db.unique/identity}] (identity-label _ ctx props)
      [i :pull nil _] (element-label _ ctx props)
      [_ :pull aa _] (ref-label _ ctx props)
      [_ :variable _ _] (element-label _ ctx props)
      [_ :aggregate _ _] (element-label _ ctx props))))


(defn ^:export semantic-css "Works at all levels: attr, element and fiddle."
  [ctx]
  ; Include the fiddle level ident css.
  ; Semantic css needs to be prefixed with - to avoid collisions. todo
  (let [[_ a _] @(:hypercrud.browser/eav ctx)]
    (->> (concat
           ["hyperfiddle"
            (context/dbname ctx)                            ; color
            (name (context/segment-type-2 a))               ; false :attribute on fiddle-ident fixme
            (string/join "/" (:hypercrud.browser/pull-path ctx)) ; legacy unique selector for each location
            (->> (:hypercrud.browser/pull-path ctx)              ; actually generate a unique selector for each location
                 (cons "-hypercrud-browser-path")                ; path prefix differentiates between attr and single attr paths
                 (string/join "/"))]
           (when (> (hypercrud.browser.context/pull-depth ctx) 0) ; differentiate pull from fiddle-ident
             [(contrib.datomic/valueType @(:hypercrud.browser/schema ctx) a)
              (contrib.datomic/cardinality @(:hypercrud.browser/schema ctx) a)
              (if (contrib.datomic/isComponent @(:hypercrud.browser/schema ctx) a) :component)])
           (:hypercrud.browser/pull-path ctx))              ; pullpath is legacy, remove from css todo
         (map css-slugify)
         (apply css))))

(defn- value-props [props ctx]
  (as-> props props
    (update props :disabled #(or % (not @(r/track writable-entity? ctx))))
    (cond-> props (context/leaf-invalid? ctx) (assoc :is-invalid true))
    (update props :class css (if (:disabled props) "disabled"))))

(defn ^:export value "Relation level value renderer. Works in forms and lists but not tables (which need head/body structure).
User renderers should not be exposed to the reaction."
  ; Path should be optional, for disambiguation only. Naked is an error
  [relative-path ctx ?f & [props]]                          ; ?f :: (ref, props, ctx) => DOM
  (let [ctx (context/focus ctx relative-path)
        props (-> (update props :class css (semantic-css ctx))
                  (value-props ctx))]
    [(or ?f hyper-control) (hypercrud.browser.context/data ctx) ctx props]))

(defn ^:export anchor [ctx props & children]
  (let [props (-> props
                  (update :class css "hyperfiddle")
                  (dissoc :route)
                  (assoc :href (some->> (:route props) (domain/url-encode (runtime/domain (:peer ctx)))))
                  (select-keys [:class :href :style]))]
    (into [:a props] children)))

(letfn [(prompt [ctx ?label]
          (or ?label
              (->> (set (context/link-class ctx))
                   (remove #(= "hf" (namespace %)))         ; "iframe" is not a good name
                   (map name)
                   (interpose " ")
                   (apply str)
                   blank->nil)
              (some-> (context/link-fiddle ctx) :fiddle/ident name)
              (some-> (context/link-tx ctx) name)))
        (link-tooltip [?route ctx] ; Show how it routed. The rest is obvious from data mode
          (if-let [[fiddle-ident args] ?route]
            (->> (concat #_[fiddle-ident] args)
                 (map pr-str) (interpose " ") (apply str))))
        (validated-route-tooltip-props [+?route link-ref ctx props] ; link is for validation
          ; this is a fine place to eval, put error message in the tooltip prop
          ; each prop might have special rules about his default, for example :visible is default true,
          ; does this get handled here?
          (let [+route (>>= +?route #(routing/validated-route+ (:link/fiddle @link-ref) % ctx))
                errors (->> [+route] (filter either/left?) (map cats/extract) (into #{}))
                ?route (unwrap (constantly nil) +route)]
            (-> props
                (assoc :route ?route)
                (update :tooltip (fn [existing-tooltip]
                                   (if-not (empty? errors)
                                     [:warning (pprint-str errors)]
                                     (if (:hyperfiddle.ui/debug-tooltips ctx)
                                       [nil (link-tooltip ?route ctx)]
                                       existing-tooltip))))
                (update :class css (when-not (empty? errors) "invalid")))))
        (disabled? [link-ref ctx]
          (condp some @(r/fmap :link/class link-ref)
            #{:hf/new} nil #_(not @(r/track security/can-create? ctx)) ; flag
            #{:hf/remove} (if (let [[_ a _] @(:hypercrud.browser/eav ctx)] a)
                            (if-let [ctx (:hypercrud.browser/parent ctx)] (not @(r/track security/writable-entity? ctx))) ; check logic
                            (not @(r/track security/writable-entity? ctx)))
            ; else we don't know the semantics, just nil out
            nil))]
  (defn ui-from-link [link-ref ctx & [props ?label]]
    {:pre [link-ref (r/reactive? link-ref)]}
    ; Once this link changes, pretty much everything below here needs to recompute
    ; Reactions are unlikely to be faster than render-tree-pruning.
    (let [visual-ctx ctx                                    ; visual-ctx should be tracked in the link context.
          +route-and-ctx (context/refocus-to-link+ ctx link-ref) ; Can fail if formula dependency isn't satisfied
          +route (fmap second +route-and-ctx)
          ?ctx (branch +route-and-ctx (constantly nil) first)
          ; Changed behavior: if the link failed to focus, there isn't a txfn or iframe now.
          has-tx-fn (boolean (context/link-tx ?ctx))
          is-iframe (some #{:hf/iframe} (context/link-class ?ctx))]
      ;(assert ?ctx "not sure what there is that can be done")
      (cond
        (and has-tx-fn (nil? (context/link-fiddle ?ctx)))
        (let [props (-> props
                        (update :class css "hyperfiddle")
                        (update :disabled #(or % (disabled? link-ref ?ctx))))]
          [effect-cmp ?ctx props (prompt ?ctx ?label)])

        (or has-tx-fn (and is-iframe (:iframe-as-popover props)))
        (let [props (-> (validated-route-tooltip-props +route link-ref ?ctx props)
                        (dissoc :iframe-as-popover)
                        (update :class css "hyperfiddle")   ; should this be in popover-cmp? unify with semantic css
                        (update :disabled #(or % (disabled? link-ref ?ctx)))
                        (assoc :hyperfiddle.ui.popover/visual-eav (context/eav visual-ctx)))
              label (prompt ?ctx ?label)]
          [popover-cmp ?ctx props label])

        is-iframe
        [stale/loading (stale/can-be-loading? visual-ctx)          ; was just ctx before
         (fmap #(route/assoc-frag % (:frag props)) +route) ; what is this frag noise?
         (fn [e]
           ; These extra error keys don't work wtf https://github.com/hyperfiddle/hyperfiddle/issues/919
           (let [E (or #_(::error-render-custom props)        ; try props first
                       #_(::error-render-custom ctx)          ; this is allowed and different than props
                       (ui-error/error-comp ?ctx))]
             [E e]))
         (fn [route]
           (let [iframe (or (::custom-iframe props) iframe-cmp)]
             [iframe ?ctx (-> props                          ; flagged - :class
                             (assoc :route route)
                             (dissoc props ::custom-iframe)
                             (update :class css (->> (context/link-class ?ctx)
                                                     (string/join " ")
                                                     css-slugify)))]))]

        :else (let [props (validated-route-tooltip-props +route link-ref ?ctx props)]
                [tooltip (tooltip-props (:tooltip props))
                 (let [props (dissoc props :tooltip)]
                   ; what about class - flagged
                   ; No eav, the route is the same info
                   [anchor ?ctx props (prompt ?ctx ?label)])])))))

(defn ^:export link "Render a link. :hf/iframes have managed loading component."
  [corcs ctx & [?label props]]
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

(defn form-field "Form fields are label AND value. Table fields are label OR value.
  EAV is already set."
  [ctx Body Head props]
  (let []
    [:div {:class (css "field" (:class props))
           :style {:border-color (domain/database-color (runtime/domain (:peer ctx)) (context/dbname ctx))}}
     [Head nil ctx props]                                   ; suppress ?v in head even if defined
     [Body (hypercrud.browser.context/data ctx) ctx (value-props props ctx)]]))

(defn table-field "Form fields are label AND value. Table fields are label OR value."
  [ctx Body Head props]                                     ; Body :: (val props ctx) => DOM, invoked as component
  (case (if (:hypercrud.browser/head-sentinel ctx) :head :body)        ; broken
    :head [:th {:class (css "field" (:class props))         ; hoist
                :style {:background-color (domain/database-color (runtime/domain (:peer ctx)) (context/dbname ctx))}}
           [Head nil ctx (-> props
                             (update :class css (when (sort/sortable? ctx) "sortable")
                                     (if-let [[p ?d] (sort/sort-directive ctx)]
                                       (if ?d
                                         (name ?d))))
                             (assoc :on-click (r/partial sort/toggle-sort! ctx)))]]
    ; Field omits [] but table does not, because we use it to specifically draw repeating anchors with a field renderer.
    :body [:td {:class (css "field" (:class props))
                :style {:border-color (domain/database-color (runtime/domain (:peer ctx)) (context/dbname ctx))}}
           [Body (hypercrud.browser.context/data ctx) ctx (value-props props ctx)]]))

(defn ^:export field "Works in a form or table context. Draws label and/or value."
  [relative-path ctx & [?f props]]
  {:pre [ctx]}
  (let [ctx (context/focus ctx relative-path) ; Handle element and attr
        Body (or ?f hyper-control)
        Head (or (:label-fn props) hyper-label)
        props (dissoc props :label-fn)
        props (update props :class css (semantic-css ctx))]


    ; Make this go away. Since field isn't an abstraction, these don't need to share code anymore?
    ; What about multimethod field overload?
    (case (:hyperfiddle.ui/layout ctx)                      ; check cardinality
      :hyperfiddle.ui.layout/table
      ^{:key (str relative-path)}
      [table-field ctx Body Head props]

      ^{:key (str relative-path)}                           ; could be a product, does eav as key work for that?
      [form-field ctx Body Head props])))

(defn ^:export table "Semantic table; columns driven externally" ; this is just a widget
  [columns ctx & [props]]
  {:pre [(s/assert :hypercrud/context ctx)]}
  ; Need backwards compat arity
  (let [sort-col (r/atom (::sort/initial-sort props))]
    (fn [columns ctx & [props]]
      {:pre [(s/assert :hypercrud/context ctx)]}
      (let [props (update props :class (fnil css "hyperfiddle") "unp") ; fnil case is iframe root (not a field :many)
            ctx (assoc ctx ::sort/sort-col sort-col
                           :hypercrud.browser/head-sentinel true ; hacks - this is coordination with the context, how to fix?
                           ::layout :hyperfiddle.ui.layout/table)]
        [:table (select-keys props [:class :style])
         [:thead (into [:tr] (columns ctx))]
         ; filter? Group-by? You can't. This is data driven. Shape your data in the peer.
         (into [:tbody] (for [[ix [?k ctx]] (->> (hypercrud.browser.context/spread-rows ctx #(sort/sort-fn % sort-col))
                                                 ; Duplicate rows https://github.com/hyperfiddle/hyperfiddle/issues/298
                                                 (map-indexed vector))]
                          ; columns keys should work out, field sets it on inside
                          #_[:tr {:key (str ?k)} (columns ctx)]
                          (let [k (or ?k ix)
                                cs (columns ctx)]
                            (into [:tr {:key (str k)}] cs))))]))))

(defn hint [val {:keys [hypercrud.browser/fiddle] :as ctx} props]
  (if (and (-> (:fiddle/type @fiddle) (= :entity))
           (empty? val))
    [:div.alert.alert-warning "Warning: invalid route (d/pull requires an entity argument). To add a tempid entity to
    the URL, click here: "
     [:a {:href "~entity('$','tempid')"} [:code "~entity('$','tempid')"]] "."]))

(defn form "Not an abstraction." [columns val ctx & [props]]
  {:pre [(s/assert :hypercrud/context ctx)]}
  (let [ctx (assoc ctx ::layout :hyperfiddle.ui.layout/block)] ; make this go away
    (into
      [:<> {:key (str (context/row-key ctx val context/entity-viewkey))}]
      (columns ctx))))

(defn columns [ctx & [props]]
  {:pre [(s/assert :hypercrud/context ctx)]}
  #_(cons (field relpath ctx nil props))
  (doall
    (for [[k _] (hypercrud.browser.context/spread-attributes ctx)]
      [field [k] ctx nil props])))

(defn pull "handles any datomic result that isn't a relation, recursively"
  [val ctx & [props]]
  {:pre [(s/assert :hypercrud/context ctx)]}
  (let [[_ a _] (context/eav ctx)]
    ; detect top and do fiddle-level here, instead of in columns?
    (match* [(contrib.datomic/cardinality @(:hypercrud.browser/schema ctx) a)] ; this is parent - not focused?
      [:db.cardinality/one] [:<>
                             (controls/hf-new val ctx)
                             (controls/hf-remove val ctx)
                             [form columns val ctx props]]

      ; http://hyperfiddle.hyperfiddle.site/:hyperfiddle.ide!domain/~entity('$domains',(:domain!ident,'hyperfiddle'))
      [:db.cardinality/many] [:<>
                              #_(controls/hf-new val ctx)   ; table db/id will handle this
                              [table columns ctx props]]
      [_] [:pre (pr-str a)])))

(defn table-column-product "We collapse the top layer of pull into a cartesian product.
  What we really want is grouped table headers to clarify what is going on here."
  [ctx props]
  ; http://hyperfiddle.hyperfiddle.site/:database!options-list/
  #_(cons (field [] ctx props))
  (->> (for [[i {el :hypercrud.browser/element :as ctx-e}] (hypercrud.browser.context/spread-elements ctx)]
         #_(cons (field [i] ctx props))
         (case (unqualify (contrib.datomic/parser-type @el))
           :variable [[field [i] ctx props]]
           :aggregate [[field [i] ctx props]]
           :pull (for [[a ctx-a] (hypercrud.browser.context/spread-attributes ctx-e)]
                   [field [i a] ctx props])))
       (mapcat identity)                                    ; Don't flatten the hiccup
       doall))

(defn ^:export result "Default result renderer. Invoked as fn, returns seq-hiccup, hiccup or
nil. call site must wrap with a Reagent component"          ; is this just hyper-control ?
  [val ctx & [props]]
  [:<>
   (doall
     (for [[k ctx] (hypercrud.browser.context/spread-result ctx)]
       [:<> {:key k}
        (hint val ctx props)
        (condp some [(type @(:hypercrud.browser/qfind ctx))] ; spread-rows

          #{FindRel FindColl}
          [table table-column-product ctx props]            ; identical result?

          #{FindTuple FindScalar}
          [form table-column-product val ctx props])]))])

;(defmethod render :hf/blank [ctx props]
;  [iframe-field-default val ctx props])
;
;(defmethod render :hf/find-rel [ctx props]
;  [table table-column-product ctx props])
;
;(defmethod render :hf/find-coll [ctx props]
;  [table table-column-product ctx props])
;
;(defmethod render :hf/find-tuple [ctx props]
;  [form table-column-product val ctx props])
;
;(defmethod render :hf/find-scalar [ctx props]
;  [:<>
;   (hint val ctx props)
;   [form table-column-product val ctx props]])

(def ^:dynamic markdown)                                    ; this should be hf-contrib or something

(def ^:export fiddle (-build-fiddle))

(defn ^:export fiddle-xray [val ctx & [props]]
  [:div.container-fluid (select-keys props [:class :on-click])
   [:h3 (pr-str @(:hypercrud.browser/route ctx))]
   ;(for [ctx (hyperfiddle.api/spread-fiddle ctx)])
   [result val ctx {}]
   #_[iframe-field-default val ctx props]])

(letfn [(render-edn [data]
          (let [edn-str (pprint-str data 160)]
            [contrib.ui/code {:value edn-str :read-only true}]))]
  (defn ^:export fiddle-api [_ ctx & [props]]
    [:div.hyperfiddle.display-mode-api.container-fluid (select-keys props [:class])
     [:h3
      [:dl
       [:dt "route"] [:dd (pr-str @(:hypercrud.browser/route ctx))]]]
     (render-edn (some-> (:hypercrud.browser/result ctx) deref))
     (->> (hyperfiddle.data/select-many ctx #{:hf/iframe}) ; this omits deep dependent iframes fixme
          (map #(base/data-from-link! % ctx))
          (concat (when @(r/fmap :fiddle/hydrate-result-as-fiddle (:hypercrud.browser/fiddle ctx))
                    (let [[_ [inner-fiddle & inner-args]] @(:hypercrud.browser/route ctx)
                          route [inner-fiddle (vec inner-args)]]
                      [(base/data-from-route! route ctx)])))
          (map (fn [ctx]
                 (let [route (some-> (:hypercrud.browser/route ctx) deref)]
                   ^{:key (str (hash route))}
                   [:div
                    [:dl [:dt "route"] [:dd (pr-str route)]]
                    (render-edn (some-> (:hypercrud.browser/result ctx) deref))])))
          doall)]))

(defn ^:export img [val ctx & [props]]
  [:img (merge props {:src val})])
