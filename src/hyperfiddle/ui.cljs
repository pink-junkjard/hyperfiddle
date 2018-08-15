(ns hyperfiddle.ui
  (:require-macros [hyperfiddle.ui :refer [-build-fiddle]])
  (:require
    [cats.core :as cats :refer [fmap mlet]]
    [cats.monad.either :as either]
    [clojure.core.match :refer [match match*]]
    [clojure.string :as string]
    [contrib.css :refer [css css-slugify]]
    [contrib.data :refer [take-to unwrap]]
    [contrib.pprint :refer [pprint-str]]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.reactive-debug :refer [track-cmp]]
    [contrib.reagent-native-events :refer [native-click-listener]]
    [contrib.string :refer [memoized-safe-read-edn-string blank->nil or-str]]
    [contrib.try$ :refer [try-either]]
    [contrib.ui]
    [contrib.ui.input]
    [contrib.ui.safe-render :refer [user-portal]]
    [contrib.ui.tooltip :refer [tooltip tooltip-props]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.fiddle :as fiddle]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.link :as link]
    [hypercrud.browser.router :as router]
    [hypercrud.browser.routing :as routing]
    [hypercrud.types.ThinEntity :refer [->ThinEntity]]
    [hypercrud.ui.connection-color :refer [border-color]]
    [hypercrud.ui.error :as ui-error]
    [hypercrud.ui.stale :as stale]
    [hyperfiddle.data :as data]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.api]
    [hyperfiddle.ui.controls :as controls]
    [hyperfiddle.ui.hyper-controls :refer [attribute-label entity-label magic-new-body magic-new-head]]
    [hyperfiddle.ui.hacks]                                  ; exports
    [hyperfiddle.ui.popover :refer [popover-cmp]]
    [hyperfiddle.ui.select :refer [select]]
    [hyperfiddle.ui.sort :as sort]
    [hyperfiddle.ui.util :refer [eval-renderer-comp]]))


(defn attr-renderer-control [val props ctx]
  ; The only way to stabilize this is for this type signature to become a react class.
  (let [?user-f @(->> (context/hydrate-attribute ctx (:hypercrud.browser/attribute ctx))
                      (r/fmap (r/comp blank->nil :attribute/renderer)))]
    (if ?user-f
      [user-portal (ui-error/error-comp ctx)
       ; ?user-f is stable due to memoizing eval (and only due to this)
       [eval-renderer-comp nil ?user-f val props ctx]])))

(defn attr-renderer [ctx]
  (if @(->> (context/hydrate-attribute ctx (:hypercrud.browser/attribute ctx))
            (r/fmap (r/comp blank->nil :attribute/renderer)))
    ; This is the only way to stabilize this.
    attr-renderer-control))

(defn ^:export control' "this is a function, which returns component" [ctx] ; returns Func[(ref, props, ctx) => DOM]
  (let [attr @(context/hydrate-attribute ctx (:hypercrud.browser/attribute ctx))]
    (let [type (some-> attr :db/valueType :db/ident name keyword)
          cardinality (some-> attr :db/cardinality :db/ident name keyword)]
      (match* [type cardinality]
        [:boolean :one] controls/boolean
        [:keyword :one] controls/keyword
        [:string :one] controls/string
        [:long :one] controls/long
        [:instant :one] controls/instant
        [:ref :one] controls/dbid                           ; nested form
        [:ref :many] (constantly [:noscript]) #_edn-many    ; nested table
        [_ :one] controls/edn
        [_ :many] controls/edn-many))))

(declare result)
(declare link)
(declare ui-from-link)
(declare fiddle-api)
(declare fiddle-xray)

(defn control [val props ctx]
  [(control' ctx) val props ctx])

(defn entity [val props ctx]                                ; What about semantics we don't understand?
  [:div #_(fragment)
   (->> (concat
          (data/select-all ctx :hyperfiddle/edit)
          (data/select-all ctx :hyperfiddle/remove)
          (data/select-all ctx :options)
          (data/select-all ctx :anchor)
          (data/select-all ctx :button)
          (data/select-all ctx :iframe))
        (remove (comp (partial data/deps-over-satisfied? (:hypercrud.browser/path ctx)) link/read-path :link/path))
        (r/track identity)
        (r/unsequence :db/id)
        (map (fn [[rv k]]
               ^{:key k}
               [ui-from-link rv ctx props])))])

(defn result+ [val props ctx]
  [result val ctx props])

(defn select+ [val props ctx]
  [select (data/select+ ctx :options (:options props)) props ctx])

(defn hyper-control' "Handles labels too because we show links there." ; CTX is done after here. props and val only. But recursion needs to look again.
  [ctx]
  {:post [%]}
  (let [head-or-body (->> (:hypercrud.browser/path ctx) (reverse) (take-to (comp not #{:head :body})) (last)) ; todo head/body attr collision
        segment (last (:hypercrud.browser/path ctx))
        segment-type (context/segment-type segment)
        child-fields (not @(r/fmap (r/comp nil? ::field/children) (:hypercrud.browser/field ctx)))]
    (match [head-or-body segment-type segment child-fields #_@user]
      [:head :attribute '* _] magic-new-head
      [:body :attribute '* _] magic-new-body

      [:head :attribute _ _] attribute-label
      [:body :attribute _ true] result+
      [:body :attribute _ false] (or (attr-renderer ctx) control)

      [:head :element _ true] entity-label
      [:body :element _ true] entity                        ; entity (:remove :edit)

      [:head :element _ false] attribute-label              ; preserve old behavior
      [:body :element _ false] controls/string              ; aggregate, what else?

      [:head :naked _ _] (r/constantly [:noscript])         ; This is the fiddle links table – nested :head independent
      [:body :naked _ _] entity
      )))

(defn ^:export hyper-control [val props ctx]
  [(hyper-control' ctx) val props ctx])

(defn auto-link-css [link]                                  ; semantic-css
  (->> (:link/class link)
       (interpose " ")
       (apply str)))

(defn ^:export semantic-css [ctx]
  ; Include the fiddle level ident css.
  ; Semantic css needs to be prefixed with - to avoid collisions. todo
  (->> (:hypercrud.browser/path ctx)
       (remove #{:head :body})
       (concat
         ["hyperfiddle"
          (:hypercrud.browser/source-symbol ctx)            ; color
          (name (context/segment-type (last (:hypercrud.browser/path ctx))))
          (or (some #{:head} (:hypercrud.browser/path ctx)) ; could be first nested in a body
              (some #{:body} (:hypercrud.browser/path ctx)))
          (->> (:hypercrud.browser/path ctx)                ; legacy unique selector for each location
               (remove #{:head :body})
               (map css-slugify)
               (string/join "/"))
          (->> (:hypercrud.browser/path ctx)                ; actually generate a unique selector for each location
               (remove #{:head :body})
               (cons :hypercrud.browser/path)               ; need to prefix the path with something to differentiate between attr and single attr paths
               (map css-slugify)
               (string/join "/"))]
         (let [attr (context/hydrate-attribute ctx (:hypercrud.browser/attribute ctx))]
           [@(r/cursor attr [:db/valueType :db/ident])
            @(r/cursor attr [:attribute/renderer])  #_label/fqn->name
            @(r/cursor attr [:db/cardinality :db/ident])
            (some-> @(r/cursor attr [:db/isComponent]) (if :component))]))
       (map css-slugify)))

(defn ^:export value "Relation level value renderer. Works in forms and lists but not tables (which need head/body structure).
User renderers should not be exposed to the reaction."
  ; Path should be optional, for disambiguation only. Naked is an error
  [relative-path ctx ?f & [props]]                          ; ?f :: (ref, props, ctx) => DOM
  (let [ctx (context/focus ctx relative-path)
        props (update props :class css (semantic-css ctx))]
    [(or ?f hyper-control) @(:hypercrud.browser/data ctx) props ctx]))

(defn ^:export anchor [ctx props & children]
  (let [props (-> props
                  (update :class css "hf-auto-nav")
                  (dissoc :route)
                  (assoc :href (some->> (:route props) (runtime/encode-route (:peer ctx)))))]
    (into [:a props] children)))

(letfn [(auto-ui-css-class [ctx]                            ; semantic css
          (css (let [ident @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/ident])]
                 ["hyperfiddle"
                  (css-slugify (some-> ident namespace))
                  (css-slugify ident)])))
        (build-wrapped-render-expr-str [user-str] (str "(fn [ctx & [class]]\n" user-str ")"))]
  (defn ui-comp [ctx & [props]]
    [user-portal (ui-error/error-comp ctx)
     (let [class (css (:class props) (auto-ui-css-class ctx))]
       (case @(:hypercrud.ui/display-mode ctx)
         :hypercrud.browser.browser-ui/user (if-let [user-renderer (:user-renderer props)]
                                              [user-renderer ctx class]
                                              (let [fiddle (:hypercrud.browser/fiddle ctx)]
                                                [eval-renderer-comp
                                                 (some-> @(r/cursor fiddle [:fiddle/cljs-ns]) blank->nil)
                                                 (some-> @(r/cursor fiddle [:fiddle/renderer]) blank->nil build-wrapped-render-expr-str)
                                                 ctx class
                                                 ; If userland crashes, reactions don't take hold, we need to reset here.
                                                 ; Cheaper to pass this as a prop than to hash everything
                                                 ; Userland will never see this param as it isn't specified in the wrapped render expr.
                                                 @(:hypercrud.browser/fiddle ctx) ; for good luck
                                                 ]))
         :hypercrud.browser.browser-ui/xray [fiddle-xray ctx class]
         :hypercrud.browser.browser-ui/api [fiddle-api ctx class]))]))

(letfn [(fiddle-css-renderer [s] [:style {:dangerouslySetInnerHTML {:__html @s}}])
        (src-mode [route ctx]
          (mlet [:let [ctx (-> (context/clean ctx)
                               (routing/route route))]
                 request @(r/apply-inner-r (r/track base/meta-request-for-fiddle ctx))
                 :let [fiddle (->> {:fiddle/type :entity
                                    :fiddle/pull-database "$"} ; turns out we dont need fiddle for much if we already know the request
                                   (fiddle/fiddle-defaults)
                                   (r/track identity))
                       ctx (-> (context/source-mode ctx)
                               (context/clean)
                               (routing/route [nil [(->ThinEntity "$" [:fiddle/ident (first (:route ctx))])]]))]]
            (base/process-results fiddle request ctx)))]
  (defn ^:export iframe [ctx {:keys [route] :as props}]
    (let [click-fn (or (:hyperfiddle.ui/iframe-on-click ctx) (constantly nil)) ; parent ctx receives click event, not child frame
          either-v (or (some-> @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :error]) either/left)
                       (if (hyperfiddle.ide.fiddles.topnav/src-mode? (get route 3))
                         (src-mode route ctx)
                         (base/data-from-route route ctx)))
          error-comp (ui-error/error-comp ctx)
          props (dissoc props :route)]
      ; todo the 3 ui fns should be what is injected, not ui-comp
      [stale/loading (stale/can-be-loading? ctx) either-v
       (fn [e]
         (let [on-click (r/partial click-fn route)]
           [native-click-listener {:on-click on-click}
            [error-comp e (css "hyperfiddle-error" (:class props) "ui")]]))
       (fn [ctx]                                            ; fresh clean ctx
         (let [on-click (r/partial click-fn (:route ctx))]
           [native-click-listener {:on-click on-click}
            (fragment
              [ui-comp ctx (update props :class css "ui")]
              [fiddle-css-renderer (r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])])]))
       (fn [ctx]
         (let [on-click (r/partial click-fn (:route ctx))]
           ; use the stale ctx's route, otherwise alt clicking while loading could take you to the new route, which is jarring
           [native-click-listener {:on-click on-click}
            (fragment
              [ui-comp ctx (update props :class css "hyperfiddle-loading" "ui")]
              [fiddle-css-renderer (r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])])]))])))

(letfn [(prompt [link-ref ?label] (str (or ?label
                                           (some-> @(r/fmap :link/class link-ref) (->> (interpose " ") (apply str)) blank->nil)
                                           (some-> @(r/fmap :link/rel link-ref) name)
                                           "_")))
        (p-build-route' [ctx link] (routing/build-route' link ctx))
        (build-link-props [route'-ref link-ref ctx props]   ; todo this function needs untangling; ui-from-route ignores most of this
          ; this is a fine place to eval, put error message in the tooltip prop
          ; each prop might have special rules about his default, for example :visible is default true, does this get handled here?
          (let [unvalidated-route' @route'-ref
                [_ args :as route] (unwrap unvalidated-route')
                validated-route' (routing/validated-route' @(r/fmap :link/fiddle link-ref) route ctx)
                errors (->> [unvalidated-route' validated-route']
                            (filter either/left?) (map cats/extract) (into #{}))]
            (merge
              ; doesn't handle tx-fn - meant for the self-link. Weird and prob bad.
              {:route (unwrap unvalidated-route')
               :tooltip (if-not (empty? errors)
                          [:warning (pprint-str errors)]
                          (if (:hyperfiddle.ui/debug-tooltips ctx)
                            [nil (pr-str args)]
                            (:tooltip props)))
               :class (->> [(if-not (empty? errors) "invalid")]
                           (remove nil?)
                           (interpose " ")
                           (apply str))})))]
  (defn ui-from-link [link-ref ctx & [props ?label]]
    {:pre [link-ref]}
    (let [ctx (context/refocus ctx (link/read-path @(r/fmap :link/path link-ref)))
          error-comp (ui-error/error-comp ctx)
          route'-ref (r/fmap (r/partial p-build-route' ctx) link-ref) ; need to re-focus from the top
          link-props @(r/track build-link-props route'-ref link-ref ctx props)] ; handles :class and :tooltip props
      (when-not (:hidden link-props)
        (let [props (-> link-props
                        (update :class css (:class props))
                        (merge (dissoc props :class :tooltip))
                        (dissoc :hidden))]
          (cond
            @(r/fmap link/popover-link? link-ref)
            (let [props (update props :class css "hf-auto-nav")] ; should this be in popover-cmp? what is this class? – unify with semantic css
              [popover-cmp link-ref ctx props @(r/track prompt link-ref ?label)])

            @(r/fmap :link/render-inline? link-ref)
            ; link-props swallows bad routes (shorts them to nil),
            ; all errors will always route through as (either/right nil)
            [stale/loading (stale/can-be-loading? ctx) (fmap #(router/assoc-frag % (:frag props)) @route'-ref) ; what is this frag noise?
             (fn [e] [error-comp e])
             (fn [route]
               [iframe ctx (-> props                        ; flagged - :class
                               (assoc :route route)
                               (dissoc props :tooltip)
                               (update :class css (css-slugify @(r/fmap auto-link-css link-ref))))])]

            :else [tooltip (tooltip-props props)
                   (let [props (dissoc props :tooltip)]
                     ; what about class - flagged
                     [anchor ctx props @(r/track prompt link-ref ?label)])]
            ))))))

(defn ^:export link "Relation level link renderer. Works in forms and lists but not tables." ; this is dumb, use a field renderer
  [rel class ctx & [?label props]]                          ; path should be optional, for disambiguation only. Naked can be hard-linked in markdown?
  (either/branch
    (data/select+ ctx rel class)
    #(vector :span %)
    (fn [link-ref]
      [ui-from-link link-ref ctx props ?label])))

(defn ^:export browse "Relation level browse. Works in forms and lists but not tables."
  [rel class ctx & [?user-renderer props]]
  (let [props (if ?user-renderer
                (assoc props :user-renderer ?user-renderer)
                props)]
    (either/branch
      (data/select+ ctx rel class)
      #(vector :span %)
      (fn [link-ref]
        [ui-from-link link-ref ctx props]))))

(defn form-field "Form fields are label AND value. Table fields are label OR value."
  ; It is the driver-fn's job to elide this field if it will be empty
  [relative-path ctx Body Head props]                       ; ?f :: (val props ctx) => DOM
  (let [state (r/atom {:hyperfiddle.ui.form/magic-new-a nil})]
    (fn [relative-path ctx Body Head props]
      (let [ctx (assoc ctx :hyperfiddle.ui.form/state state)
            body-ctx (context/focus ctx (cons :body relative-path))
            head-ctx (context/focus ctx (cons :head relative-path))
            props (update props :class css (semantic-css body-ctx))]
        [:div {:class (css "field" (:class props))
               :style {:border-color (border-color body-ctx)}} ; wrapper div has :body stypes - why?
         (fragment
           ^{:key :form-head}
           [Head nil props head-ctx]
           ^{:key :form-body}
           [:div
            [Body @(:hypercrud.browser/data body-ctx) props body-ctx]])]))))

(defn table-field "Form fields are label AND value. Table fields are label OR value."
  [relative-path ctx Body Head props]                       ; Body :: (val props ctx) => DOM, invoked as component
  (let [head-or-body (last (:hypercrud.browser/path ctx))   ; this is ugly and not uniform with form-field
        ctx (context/focus ctx relative-path)
        props (update props :class css (semantic-css ctx))]
    (case head-or-body
      :head [:th {:class (css "field" (:class props)
                              (when (sort/sortable? ctx) "sortable") ; hoist
                              (some-> (sort/sort-direction relative-path ctx) name)) ; hoist
                  :style {:background-color (border-color ctx)}
                  :on-click (r/partial sort/toggle-sort! relative-path ctx)}
             [Head nil props ctx]]
      :body [:td {:class (css "field" (:class props) "truncate")
                  :style {:border-color (when (:hypercrud.browser/source-symbol ctx) (border-color ctx))}}
             [Body @(:hypercrud.browser/data ctx) props ctx]])))

; (defmulti field ::layout)
(defn ^:export field "Works in a form or table context. Draws label and/or value."
  [relative-path ctx & [?f props]]                          ; ?f :: (ref, props, ctx) => DOM
  (let [Body (or ?f hyper-control)
        Head (or (:label-fn props) hyper-control)
        props (dissoc props :label-fn)
        ; add semantic css here?
        is-magic-new (= '* (last relative-path))]
    (case (:hyperfiddle.ui/layout ctx)
      :hyperfiddle.ui.layout/table (when-not is-magic-new
                                     ^{:key (str relative-path)}
                                     [table-field relative-path ctx Body Head props])
      (let [magic-new-key (when is-magic-new
                            (let [ctx (context/focus ctx (cons :body relative-path))]
                              ; guard against crashes for nil data
                              (hash (some->> ctx :hypercrud.browser/parent :hypercrud.browser/data (r/fmap keys) deref))))]
        ^{:key (str relative-path magic-new-key #_"reset magic new state")}
        [form-field relative-path ctx Body Head props]))))

(defn ^:export table "Semantic table"                       ; this is just a widget
  [form ctx & [props]]
  (let [sort-col (r/atom nil)
        sort (fn [v] (hyperfiddle.ui.sort/sort-fn v sort-col))]
    (fn [form ctx & [props]]
      (let [ctx (assoc ctx ::sort/sort-col sort-col
                           ::layout :hyperfiddle.ui.layout/table)]
        [:table (update props :class (fnil css "hyperfiddle") "ui-table" "unp") ; fnil case is iframe root (not a field :many)
         (let [ctx (context/focus ctx [:head])]
           (->> (form ctx) (into [:thead])))                ; strict
         (->> (:hypercrud.browser/data ctx)
              (r/fmap sort)
              (r/unsequence data/row-keyfn)
              (map (fn [[row k]]
                     (->> (form (context/body ctx row))
                          (into ^{:key k} [:tr]))))         ; strict
              (into [:tbody]))]))))

(defn hint [{:keys [hypercrud.browser/fiddle
                    hypercrud.browser/data]}]
  (if (and (-> (:fiddle/type @fiddle) (= :entity))
           (nil? (:db/id @data)))
    [:div.alert.alert-warning "Warning: invalid route (d/pull requires an entity argument). To add a tempid entity to the URL, click here: "
     [:a {:href "~entity('$','tempid')"} [:code "~entity('$','tempid')"]] "."]))

(letfn [(field-with-props [props relative-path ctx] (field relative-path ctx nil props))] ; dissoc class ?
  (defn ^:export result "Default result renderer. Invoked as fn, returns seq-hiccup, hiccup or
nil. call site must wrap with a Reagent component"
    [val ctx & [props]]
    ; focus should probably get called here. What about the request side?
    (fragment
      (hint ctx)
      (case @(r/fmap ::field/cardinality (:hypercrud.browser/field ctx))
        :db.cardinality/one (let [ctx (assoc ctx ::layout :hyperfiddle.ui.layout/block)
                                  key (-> (data/row-keyfn val) str keyword)]
                              (apply fragment key (data/form (r/partial field-with-props props) ctx)))
        :db.cardinality/many [table (r/partial data/form (r/partial field-with-props props)) ctx props] ; Inherit parent class, either a field renderer (nested) or nothing (root)
        ; blank fiddles
        nil)
      (->> (concat (data/select-all ctx :options)
                   (data/select-all ctx :iframe)
                   (data/select-all ctx :anchor)
                   (data/select-all ctx :button))
           (remove (comp (partial data/deps-over-satisfied? (:hypercrud.browser/path ctx)) link/read-path :link/path))
           (r/track identity)
           (r/unsequence :db/id)
           (map (fn [[rv k]]
                  ^{:key k}
                  [ui-from-link rv ctx props]))))))

(def ^:dynamic markdown)                                    ; this should be hf-contrib or something

(def ^:export fiddle (-build-fiddle))

(defn ^:export fiddle-xray [ctx class]
  (let [{:keys [:hypercrud.browser/fiddle]} ctx]
    [:div {:class class}
     [:h3 (pr-str (:route ctx)) #_(some-> @fiddle :fiddle/ident str)]
     (result @(:hypercrud.browser/data ctx) ctx #_{:class (css (semantic-css ctx))})]))

(letfn [(render-edn [data]
          (let [edn-str (-> (hyperfiddle.ui.hacks/pull-soup->tree data)
                            (pprint-str 160))]
            [contrib.ui/code edn-str #() {:read-only true}]))]
  (defn ^:export fiddle-api [ctx class]
    (let [data (hyperfiddle.ui.api/api-data ctx)]
      [:div.hyperfiddle.display-mode-api {:class class}
       [:h3
        [:dl
         [:dt "route"] [:dd (pr-str (:route ctx))]]]
       (render-edn (get data (:route ctx)))
       (->> (dissoc data (:route ctx))
            (map (fn [[route result]]
                   ^{:key (str (hash route))}
                   [:div
                    [:dl [:dt "route"] [:dd (pr-str route)]]
                    (render-edn result)])))])))

(defn ^:export img [val props ctx]
  [:img (merge props {:src val})])
