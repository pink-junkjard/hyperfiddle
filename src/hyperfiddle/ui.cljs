(ns hyperfiddle.ui
  (:require-macros [hyperfiddle.ui :refer [-build-fiddle]])
  (:require
    [cats.core :as cats :refer [fmap mlet]]
    [cats.monad.either :as either :refer [branch]]
    [clojure.core.match :refer [match match*]]
    [clojure.string :as string]
    [contrib.ct :refer [unwrap]]
    [contrib.css :refer [css css-slugify]]
    [contrib.data :refer [take-to]]
    [contrib.pprint :refer [pprint-str]]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.reagent-native-events :refer [native-click-listener]]
    [contrib.string :refer [blank->nil]]
    [contrib.ui]
    [contrib.ui.input]
    [contrib.ui.safe-render :refer [user-portal]]
    [contrib.ui.tooltip :refer [tooltip tooltip-props]]
    [datascript.parser]
    [hypercrud.browser.auto-link :refer [auto-link]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.fiddle :as fiddle]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.link :as link]
    [hypercrud.browser.router :as router]
    [hypercrud.browser.routing :as routing]
    [hypercrud.browser.system-link :refer [console-links system-link?]]
    [hypercrud.types.ThinEntity :refer [->ThinEntity]]
    [hypercrud.ui.connection-color :refer [border-color]]
    [hypercrud.ui.error :as ui-error]
    [hypercrud.ui.stale :as stale]
    [hyperfiddle.data :as data]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.api]
    [hyperfiddle.ui.controls :as controls]
    [hyperfiddle.ui.hyper-controls :refer [attribute-label entity-label dbid-label magic-new-body magic-new-head]]
    [hyperfiddle.ui.popover :refer [popover-cmp]]
    [hyperfiddle.ui.select$]
    [hyperfiddle.ui.sort :as sort]
    [hyperfiddle.ui.util :refer [eval-renderer-comp]]
    [taoensso.timbre :as timbre]))


(defn attr-renderer-control [val ctx & [props]]
  ; The only way to stabilize this is for this type signature to become a react class.
  (when-let [?user-f @(->> (context/hydrate-attribute ctx (last (:hypercrud.browser/path ctx)))
                           (r/fmap (r/comp blank->nil :attribute/renderer)))]
    [user-portal (ui-error/error-comp ctx)
     ; ?user-f is stable due to memoizing eval (and only due to this)
     [eval-renderer-comp nil ?user-f val ctx props]]))

(declare result)
(declare pull)
(declare entity-links)
(declare field)
(declare hyper-control)
(declare link)
(declare ui-from-link)
(declare fiddle-api)
(declare fiddle-xray)

(def select hyperfiddle.ui.controls/ref)                    ; legacy
(def select+ hyperfiddle.ui.controls/ref)                   ; legacy

(defn entity-links-iframe [val ctx & [props]]
  (fragment
    (->> (data/select-all ctx :hf/iframe)
         (remove (comp (partial data/deps-over-satisfied? ctx) link/read-path :link/path))
         (r/track identity)
         (r/unsequence :db/id)
         (map (fn [[rv k]]
                ^{:key k}
                [ui-from-link rv ctx props]))
         doall)))

(defn display-mode-console? [ctx]
  (= :hypercrud.browser.browser-ui/xray @(:hypercrud.ui/display-mode ctx)))

(defn entity-identity-label [val]
  (pr-str (or (:db/ident val) (:db/id val))))

(defn entity-links [val ctx & [props]]                      ; rename to entity-identity
  ; What about aggregate etc?
  [:div
   ; Self link
   (case @(:hypercrud.ui/display-mode ctx)
     :hypercrud.browser.browser-ui/xray (-> (data/select-all ctx :hf/edit #{:hf.ide/console})
                                            (as-> links (first (filter (comp (partial = (:hypercrud.browser/path ctx)) link/read-path :link/path) links)))
                                            (as-> link [ui-from-link (r/track identity link) ctx props (entity-identity-label val)]))

     ; This is wrong - must show user links in xray mode
     :hypercrud.browser.browser-ui/user (let [self (-> (data/select-all ctx :hf/edit)
                                                       (->> (filter (comp (partial = (:hypercrud.browser/path ctx)) link/read-path :link/path)))
                                                       first)
                                              label (entity-identity-label val)]
                                          (if self
                                            [ui-from-link (r/track identity self) ctx props label]
                                            [:div label])))

   ; Remove link
   (if (display-mode-console? ctx)
     (-> (data/select-all ctx :hf/remove #{:hf.ide/console})
         (as-> links (first (filter (comp (partial = (:hypercrud.browser/path ctx)) link/read-path :link/path) links)))
         (as-> link [ui-from-link (r/track identity link) ctx props "remove"])))])

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

      [:splat _] magic-new-body
      [:boolean :one] controls/boolean
      [:keyword :one] controls/keyword
      [:string :one] controls/string
      [:long :one] controls/long
      [:instant :one] controls/instant
      [:ref :one] controls/ref                              ; nested form - and causes recursion
      [:ref :many] controls/edn-many                        ; multi select?
      [_ :one] controls/edn
      [_ :many] controls/edn-many)))

(defn children-identity-only? [field]
  (->> (::field/children field)
       (remove (comp (partial = :db/ident) ::field/path-segment))
       (remove (comp (partial = :db/id) ::field/path-segment))
       empty?))

(defn ^:export hyper-control [val ctx & [props]]
  {:post [%]}
  (or (attr-renderer-control val ctx props)
      (let [-field @(:hypercrud.browser/field ctx)
            segment (last (:hypercrud.browser/path ctx))]
        (cond                                               ; Duplicate options test to avoid circular dependency in controls/ref
          (:options props) [(control val ctx props) val ctx props]
          (#{:db/id :db/ident} segment) [controls/dbid val ctx props]
          (children-identity-only? -field) [(control val ctx props) val ctx props]
          (seq (::field/children -field)) (let [ctx (dissoc ctx ::layout)]
                                            (fragment
                                              [pull field val ctx props]
                                              [field [] ctx entity-links-iframe (assoc props :label-fn (r/constantly nil #_[:div "nested pull iframes"]))]))
          :else [(control val ctx props) val ctx props]))))

(defn hyper-label [_ ctx & [props]]
  (let [level (-> ctx :hypercrud.browser/field deref ::field/level)
        segment (last (:hypercrud.browser/path ctx))
        segment-type (context/segment-type-2 segment)       ; :element means :relation? no, naked. Relation is ortho
        child-fields (not @(r/fmap (r/comp nil? ::field/children) (:hypercrud.browser/field ctx)))]
    (cond
      (#{:db/id :db/ident} segment) dbid-label
      :else (match* [level segment-type segment child-fields #_@user]
              [nil :splat _ _] magic-new-head
              [nil :attribute _ _] attribute-label          ; entity-[] ends up here
              [nil :element _ true] entity-label
              [nil :element _ false] attribute-label        ; preserve old behavior
              [nil :naked-or-element _ _] entity-label      ; Schema new attr, and fiddle-links new link - needs to be split
              [:relation :naked-or-element _ _] (r/constantly [:label "*relation*"])
              [:tuple :naked-or-element _ _] (r/constantly [:label "*tuple*"])))))

(defn auto-link-css [link]                                  ; semantic-css
  (->> (:link/class link)
       (interpose " ")
       (apply str)))

(defn ^:export semantic-css [ctx]
  ; Include the fiddle level ident css.
  ; Semantic css needs to be prefixed with - to avoid collisions. todo
  (->> (:hypercrud.browser/path ctx)
       (concat
         ["hyperfiddle"
          (context/dbname ctx)                              ; color
          (name (context/segment-type-2 (last (:hypercrud.browser/path ctx))))
          (->> (:hypercrud.browser/path ctx)                ; legacy unique selector for each location
               (map css-slugify)
               (string/join "/"))
          (->> (:hypercrud.browser/path ctx)                ; actually generate a unique selector for each location
               (cons :hypercrud.browser/path)               ; need to prefix the path with something to differentiate between attr and single attr paths
               (map css-slugify)
               (string/join "/"))]
         (when (context/attribute-segment? (last (:hypercrud.browser/path ctx)))
           (let [attr (context/hydrate-attribute ctx (last (:hypercrud.browser/path ctx)))]
             [@(r/cursor attr [:db/valueType :db/ident])
              @(r/cursor attr [:attribute/renderer])  #_label/fqn->name
              @(r/cursor attr [:db/cardinality :db/ident])
              (some-> @(r/cursor attr [:db/isComponent]) (if :component))])))
       (map css-slugify)))

(defn ^:export value "Relation level value renderer. Works in forms and lists but not tables (which need head/body structure).
User renderers should not be exposed to the reaction."
  ; Path should be optional, for disambiguation only. Naked is an error
  [relative-path ctx ?f & [props]]                          ; ?f :: (ref, props, ctx) => DOM
  (let [ctx (context/focus ctx relative-path)
        props (update props :class css (semantic-css ctx))]
    [(or ?f hyper-control) @(:hypercrud.browser/data ctx) ctx props]))

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
        (build-wrapped-render-expr-str [user-str] (str "(fn [val ctx props]\n" user-str ")"))
        (ui-comp [ctx & [props]]                            ; user-renderer comes through here
          [user-portal (ui-error/error-comp ctx)
           (let [props' (update props :class css (auto-ui-css-class ctx))
                 props (select-keys props' [:class])
                 value @(:hypercrud.browser/data ctx)]
             (case @(:hypercrud.ui/display-mode ctx)
               :hypercrud.browser.browser-ui/user (if-let [user-renderer (:user-renderer props')]
                                                    [user-renderer value ctx props]
                                                    (let [fiddle (:hypercrud.browser/fiddle ctx)]
                                                      [eval-renderer-comp
                                                       (some-> @(r/cursor fiddle [:fiddle/cljs-ns]) blank->nil)
                                                       (some-> @(r/cursor fiddle [:fiddle/renderer]) blank->nil build-wrapped-render-expr-str)
                                                       value ctx props
                                                       ; If userland crashes, reactions don't take hold, we need to reset here.
                                                       ; Cheaper to pass this as a prop than to hash everything
                                                       ; Userland will never see this param as it isn't specified in the wrapped render expr.
                                                       @(:hypercrud.browser/fiddle ctx) ; for good luck
                                                       ]))
               :hypercrud.browser.browser-ui/xray [fiddle-xray value ctx props]
               :hypercrud.browser.browser-ui/api [fiddle-api value ctx props]))])
        (fiddle-css-renderer [s] [:style {:dangerouslySetInnerHTML {:__html @s}}])
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
  (defn ^:export iframe [ctx {:keys [route] :as props}]     ; :: [route ctx & [?f props]]
    (let [click-fn (or (:hyperfiddle.ui/iframe-on-click ctx) (constantly nil)) ; parent ctx receives click event, not child frame
          either-v (or (some-> @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :error]) either/left)
                       (if (hyperfiddle.ide.fiddles.topnav/src-mode? (get route 3))
                         (src-mode route ctx)
                         (base/data-from-route route ctx)))
          error-comp (ui-error/error-comp ctx)
          props (dissoc props :route)]
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

(letfn [(prompt [link-ref ?label]
          (or ?label (->> (conj (set @(r/fmap :link/class link-ref))
                                @(r/fmap :link/rel link-ref))
                          (interpose " ") (apply str) blank->nil)))]
  (defn ui-from-link [link-ref ctx & [props ?label]]
    {:pre [link-ref]}
    (let [visual-ctx ctx
          ctx (context/refocus ctx (link/read-path @(r/fmap :link/path link-ref)))
          error-comp (ui-error/error-comp ctx)
          r+route (r/fmap (r/partial routing/build-route' ctx) link-ref) ; need to re-focus from the top
          link-props @(r/track routing/build-link-props @r+route ctx props)] ; handles :class and :tooltip props
      (when-not (:hidden link-props)
        (let [style {:color nil #_(border-color ctx (cond
                                                      (system-link? (:db/id @link-ref)) 60
                                                      :else 40))}
              props (-> link-props
                        (assoc :style style)
                        (update :class css (:class props))
                        (merge (dissoc props :class :tooltip))
                        (dissoc :hidden))]
          (cond
            @(r/fmap (comp boolean :link/tx-fn) link-ref)
            (let [props (update props :class css "hf-auto-nav")] ; should this be in popover-cmp? what is this class? – unify with semantic css
              [popover-cmp link-ref ctx visual-ctx props @(r/track prompt link-ref ?label)])

            @(r/fmap (r/comp (r/partial = :hf/iframe) :link/rel) link-ref)
            ; link-props swallows bad routes (shorts them to nil),
            ; all errors will always route through as (either/right nil)
            [stale/loading (stale/can-be-loading? ctx) (fmap #(router/assoc-frag % (:frag props)) @r+route) ; what is this frag noise?
             (fn [e] [error-comp e])
             (fn [route]
               (let [iframe (or (::custom-iframe props) iframe)]
                 [iframe ctx (-> props                      ; flagged - :class
                                 (assoc :route route)
                                 (dissoc props :tooltip ::custom-iframe)
                                 (update :class css (css-slugify @(r/fmap auto-link-css link-ref))))]))]

            :else [tooltip (tooltip-props (:tooltip props))
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
  {:pre [ctx]}
  (let [props (if ?user-renderer
                (assoc props :user-renderer ?user-renderer)
                props)]
    (either/branch
      (data/select+ ctx rel class)
      #(vector :span %)
      (fn [link-ref]
        [ui-from-link link-ref ctx props]))))

(defn form-field "Form fields are label AND value. Table fields are label OR value."
  [relative-path ctx Body Head props]
  (let [state (r/atom {:hyperfiddle.ui.form/magic-new-a nil})]
    (fn [relative-path ctx Body Head props]
      (let [ctx (assoc ctx :hyperfiddle.ui.form/state state)]
        [:div {:class (css "field" (:class props))
               :style {:border-color (border-color ctx)}}
         [Head nil (dissoc ctx :hypercrud.browser/data) props]
         [Body @(:hypercrud.browser/data ctx) ctx props]]))))

(defn table-field "Form fields are label AND value. Table fields are label OR value."
  [relative-path ctx Body Head props]                       ; Body :: (val props ctx) => DOM, invoked as component
  ; Presence of data to detect head vs body? Kind of dumb
  (case (if (:hypercrud.browser/data ctx) :body :head)      ; this is ugly and not uniform with form-field NOTE: NO DEREF ON THIS NIL CHECK
    :head [:th {:class (css "field" (:class props)
                            (when (sort/sortable? ctx) "sortable") ; hoist
                            (some-> (sort/sort-direction relative-path ctx) name)) ; hoist
                :style {:background-color (border-color ctx)}
                :on-click (r/partial sort/toggle-sort! relative-path ctx)}
           [Head nil ctx props]]
    ; Field omits [] but table does not, because we use it to specifically draw repeating anchors with a field renderer.
    :body [:td {:class (css "field" (:class props))
                :style {:border-color (border-color ctx)}}
           [Body @(:hypercrud.browser/data ctx) ctx props]]))

(defn ^:export field "Works in a form or table context. Draws label and/or value."
  [relative-path ctx & [?f props]]
  (let [ctx (context/focus ctx relative-path)
        Body (or ?f hyper-control)
        Head (or (:label-fn props) hyper-label)
        props (dissoc props :label-fn)
        props (update props :class css (semantic-css ctx))
        is-magic-new (= '* (last relative-path))]
    (case (:hyperfiddle.ui/layout ctx)
      :hyperfiddle.ui.layout/table (when-not is-magic-new
                                     [table-field relative-path ctx Body Head props])
      (with-meta
        [form-field relative-path ctx Body Head props]
        (when is-magic-new
          ; guard against crashes for nil data
          {:key (hash (some->> ctx :hypercrud.browser/parent :hypercrud.browser/data (r/fmap keys) deref))})))))

(defn ^:export table "Semantic table; columns driven externally" ; this is just a widget
  [columns ctx & [props]]
  (let [sort-col (r/atom nil)
        sort (fn [v] (hyperfiddle.ui.sort/sort-fn v sort-col))]
    (fn [columns ctx & [props]]
      (let [ctx (assoc ctx ::sort/sort-col sort-col
                           ::layout :hyperfiddle.ui.layout/table)]
        [:table (update props :class (fnil css "hyperfiddle") "unp") ; fnil case is iframe root (not a field :many)
         [:thead (->> (columns (dissoc ctx :hypercrud.browser/data) props) (into [:tr]))] ; strict
         (->> (:hypercrud.browser/data ctx)
              (r/fmap sort)
              (r/unsequence data/row-keyfn)
              (map (fn [[row k]]
                     (->> (columns (context/row ctx row) props)
                          (into ^{:key k} [:tr]))))         ; strict
              (into [:tbody]))]))))

(defn hint [val {:keys [hypercrud.browser/fiddle]} props]
  (if (and (-> (:fiddle/type @fiddle) (= :entity))
           (nil? (:db/id val)))
    [:div.alert.alert-warning "Warning: invalid route (d/pull requires an entity argument). To add a tempid entity to the URL, click here: "
     [:a {:href "~entity('$','tempid')"} [:code "~entity('$','tempid')"]] "."]))

(defn form "Not an abstraction." [fields val ctx & [props]]
  (apply
    fragment
    (keyword (str (data/row-keyfn val)))
    (fields (assoc ctx ::layout :hyperfiddle.ui.layout/block))))

(defn columns [field ctx & [props]]
  (concat
    (->> (-> ctx :hypercrud.browser/field deref ::field/children)
         (map (fn [{segment ::field/path-segment}]
                ^{:key (str [segment])}
                [field [segment] ctx hyper-control props])))))

(defn columns-relation-product [field ctx & [props]]
  (concat
    (->> @(r/fmap ::field/children (:hypercrud.browser/field ctx))
         (mapcat (fn [{segment ::field/path-segment
                       child-fields ::field/children
                       el-type ::field/element-type}]
                   (concat
                     (map (fn [{child-segment ::field/path-segment}]
                            ^{:key (str [segment child-segment])}
                            [field [segment child-segment] ctx hyper-control props])
                          child-fields)
                     (if-let [f (condp = el-type
                                  datascript.parser.Variable hyper-control
                                  datascript.parser.Aggregate hyper-control
                                  datascript.parser.Pull nil #_entity-links)]
                       [^{:key (str [segment])} [field [segment] ctx f props]])))))))

(defn pull "handles any datomic result that isn't a relation, recursively"
  [field val ctx & [props]]
  (let [cardinality @(r/fmap ::field/cardinality (:hypercrud.browser/field ctx))]
    (match* [cardinality]
      [:db.cardinality/one] [form (r/partial columns field) val ctx props]
      [:db.cardinality/many] [table (r/partial columns field) ctx props])))

(defn ^:export result "Default result renderer. Invoked as fn, returns seq-hiccup, hiccup or
nil. call site must wrap with a Reagent component"          ; is this just hyper-control ?
  [val ctx & [props]]
  (fragment
    (hint val ctx props)
    (let [type @(r/fmap :fiddle/type (:hypercrud.browser/fiddle ctx))
          level @(r/fmap ::field/level (:hypercrud.browser/field ctx))]
      (match* [type level]
        [:blank _] nil
        [:query :relation] [table (r/partial columns-relation-product field) ctx props]
        [:query :tuple] [form (r/partial columns-relation-product field) val ctx props]
        [_ _] (pull field val ctx props)))

    ; Unify with pull? What about table iframes
    [field [] ctx entity-links-iframe (assoc props :label-fn (r/constantly nil #_[:div "result iframes"]))]))

(def ^:dynamic markdown)                                    ; this should be hf-contrib or something

(def ^:export fiddle (-build-fiddle))

(defn ^:export fiddle-xray [val ctx & [props]]
  (let [{:keys [:hypercrud.browser/fiddle]} ctx
        console-links (->> (console-links @fiddle @(:hypercrud.browser/field ctx) @(:hypercrud.browser/schemas ctx))
                           (map (partial auto-link ctx)))
        ctx (update ctx :hypercrud.browser/links (partial r/fmap (r/partial concat console-links)))]
    [:div (select-keys props [:class])
     [:h3 (pr-str (:route ctx))]
     (result val ctx {})]))

(letfn [(render-edn [data]
          (let [edn-str (pprint-str data 160)]
            [contrib.ui/code {:value edn-str :read-only true}]))]
  (defn ^:export fiddle-api [val ctx & [props]]
    (let [data (hyperfiddle.ui.api/api-data ctx)]
      [:div.hyperfiddle.display-mode-api (select-keys props [:class])
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

(defn ^:export img [val ctx & [props]]
  [:img (merge props {:src val})])
