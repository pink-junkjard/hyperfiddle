(ns hyperfiddle.ui
  (:require-macros [hyperfiddle.ui :refer [-build-fiddle]])
  (:require
    [cats.core :as cats :refer [fmap]]
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
    [hypercrud.browser.field :as field]
    [hypercrud.browser.link :as link]
    [hypercrud.browser.router :as router]
    [hypercrud.browser.routing :as routing]
    [hypercrud.ui.connection-color :refer [border-color]]
    [hypercrud.ui.error :as ui-error]
    [hypercrud.ui.stale :as stale]
    [hyperfiddle.data :as data]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.api]
    [hyperfiddle.ui.controls :as controls]
    [hyperfiddle.ui.hyper-controls :refer [hyper-label hyper-select-head magic-new-body magic-new-head]]
    [hyperfiddle.ui.hacks]                                  ; exports
    [hyperfiddle.ui.link-impl :as ui-link :refer [anchors iframes]]
    [hyperfiddle.ui.popover :refer [popover-cmp]]
    [hyperfiddle.ui.select :refer [select]]
    [hyperfiddle.ui.sort :as sort]
    [hyperfiddle.ui.util :refer [eval-renderer-comp+]]))


(defn attr-renderer-control [val props ctx]
  ; The only way to stabilize this is for this type signature to become a react class.
  (let [?user-f @(->> (context/hydrate-attribute ctx (:hypercrud.browser/attribute ctx))
                      (r/fmap (r/comp eval-renderer-comp+ blank->nil :attribute/renderer)))]
    (if ?user-f
      [user-portal (ui-error/error-comp ctx)
       ; ?user-f is stable due to memoizing eval (and only due to this)
       [?user-f val props ctx]])))

(defn attr-renderer [ctx]
  (if @(->> (context/hydrate-attribute ctx (:hypercrud.browser/attribute ctx))
            (r/fmap (r/comp eval-renderer-comp+ blank->nil :attribute/renderer)))
    ; This is the only way to stabilize this.
    attr-renderer-control))

(defn ^:export control "this is a function, which returns component" [ctx] ; returns Func[(ref, props, ctx) => DOM]
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

(defn control+ [val props ctx]
  (fragment
    [(control ctx) val props ctx]
    [anchors (:hypercrud.browser/path ctx) props ctx]       ; Order sensitive, here be floats
    [iframes (:hypercrud.browser/path ctx) props ctx]))

(defn links-only+ [val props ctx]
  (fragment
    [anchors (:hypercrud.browser/path ctx) props ctx]       ; Order sensitive, here be floats
    [iframes (:hypercrud.browser/path ctx) props ctx]))

(defn result+ [val props ctx]
  (fragment
    [result ctx]                                            ; flipped args :(
    [anchors (:hypercrud.browser/path ctx) props ctx]       ; Order sensitive, here be floats
    [iframes (:hypercrud.browser/path ctx) props ctx]))

(defn select+ [val props ctx]
  (fragment
    [anchors (:hypercrud.browser/path ctx) props ctx ui-link/options-processor] ; Order sensitive, here be floats
    [select props ctx]
    [iframes (:hypercrud.browser/path ctx) props ctx ui-link/options-processor]))

(defn ^:export hyper-control "Handles labels too because we show links there." ; CTX is done after here. props and val only. But recursion needs to look again.
  [ctx]
  {:post [%]}
  (let [head-or-body (->> (:hypercrud.browser/path ctx) (reverse) (take-to (comp not #{:head :body})) (last)) ; todo head/body attr collision
        rels (->> (:hypercrud.browser/links ctx)
                  (r/fmap (fn [links]
                            (->> links
                                 (filter (r/partial link/draw-link? (:hypercrud.browser/path ctx)))
                                 (map :link/rel)
                                 (into #{})))))
        segment (last (:hypercrud.browser/path ctx))
        segment-type (context/segment-type segment)
        child-fields (not @(r/fmap (r/comp nil? ::field/children) (:hypercrud.browser/field ctx)))]
    (match* [head-or-body segment-type segment child-fields @rels #_@user]
      ;[:head _ true] hyper-select-head
      [:head :attribute '* _ _] magic-new-head
      [:head _ _ _ _] hyper-label
      [:body :attribute '* _ _] magic-new-body
      [:body :attribute _ _ (true :<< #(contains? % :options))] select+
      [:body :attribute _ true _] result+
      [:body :attribute _ false _] (or (attr-renderer ctx) control+)
      [:body _ _ true _] links-only+                        ; what?
      [:body _ _ false _] controls/string                   ; aggregate, entity, what else?
      )))

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
    [(or ?f (hyper-control ctx)) @(:hypercrud.browser/data ctx) props ctx]))

(defn ^:export anchor [ctx props & children]
  (let [props (-> props
                  (update :class css "hf-auto-nav")
                  (dissoc :route)
                  (assoc :href (some->> (:route props) (runtime/encode-route (:peer ctx)))))]
    (into [:a props] children)))

(letfn [(fiddle-css-renderer [s] [:style {:dangerouslySetInnerHTML {:__html @s}}])]
  (defn ^:export iframe [ctx {:keys [route] :as props}]
    (let [click-fn (or (:hypercrud.browser/page-on-click ctx) (constantly nil)) ; parent ctx receives click event, not child frame
          either-v (or (some-> @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :error]) either/left)
                       (base/data-from-route route ctx))
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
              [(:alpha.hypercrud.browser/ui-comp ctx) ctx (update props :class css "ui")]
              [fiddle-css-renderer (r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])])]))
       (fn [ctx]
         (let [on-click (r/partial click-fn (:route ctx))]
           ; use the stale ctx's route, otherwise alt clicking while loading could take you to the new route, which is jarring
           [native-click-listener {:on-click on-click}
            (fragment
              [(:alpha.hypercrud.browser/ui-comp ctx) ctx (update props :class css "hyperfiddle-loading" "ui")]
              [fiddle-css-renderer (r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])])]))])))

(letfn [(prompt [link-ref ?label] (str (or ?label (some-> @(r/fmap :link/rel link-ref) name) "_")))
        (p-build-route' [ctx link] (routing/build-route' link ctx))
        (build-link-props [route'-ref link-ref ctx]         ; todo this function needs untangling; ui-from-route ignores most of this
          ; this is a fine place to eval, put error message in the tooltip prop
          ; each prop might have special rules about his default, for example :visible is default true, does this get handled here?
          (let [unvalidated-route' @route'-ref
                [_ args :as route] (unwrap unvalidated-route')
                validated-route' (routing/validated-route' @(r/fmap :link/fiddle link-ref) route ctx)
                user-props' (link/eval-hc-props @(r/fmap :hypercrud/props link-ref) ctx)
                user-props (unwrap user-props')
                errors (->> [user-props' unvalidated-route' validated-route']
                            (filter either/left?) (map cats/extract) (into #{}))]
            (merge
              user-props                                    ; e.g. disabled, tooltip, style, class - anything, it gets passed to a renderer maybe user renderer
              ; doesn't handle tx-fn - meant for the self-link. Weird and prob bad.
              {:route (unwrap unvalidated-route')
               :tooltip (if-not (empty? errors)
                          [:warning (pprint-str errors)]
                          (if (:hyperfiddle.ui/debug-tooltips ctx)
                            [nil (pr-str args)]
                            (:tooltip user-props)))
               :class (->> [(:class user-props)
                            (if-not (empty? errors) "invalid")]
                           (remove nil?)
                           (interpose " ")
                           (apply str))})))]
  (defn ui-from-link [link-ref ctx & [props ?label]]
    (let [error-comp (ui-error/error-comp ctx)
          route'-ref (r/fmap (r/partial p-build-route' ctx) link-ref)
          link-props @(r/track build-link-props route'-ref link-ref ctx)]
      (when-not (:hidden link-props)
        (let [props (-> link-props
                        (update :class css (:class props))
                        (merge (dissoc props :class))
                        (dissoc :hidden))]
          (cond
            @(r/fmap link/popover-link? link-ref)
            (let [props (update props :class css "hf-auto-nav")] ; should this be in popover-cmp? what is this class?
              [popover-cmp link-ref ctx props @(r/track prompt link-ref ?label)])

            @(r/fmap :link/render-inline? link-ref)
            ; link-props swallows bad routes (shorts them to nil),
            ; all errors will always route through as (either/right nil)
            [stale/loading (stale/can-be-loading? ctx) (fmap #(router/assoc-frag % (:frag props)) @route'-ref) ; what is this frag noise?
             (fn [e] [error-comp e])
             (fn [route]
               [iframe ctx (-> props
                               (assoc :route route)
                               (dissoc props :tooltip)
                               (update :class css (css-slugify @(r/fmap :link/rel link-ref))))])]

            :else [tooltip (tooltip-props props)
                   (let [props (dissoc props :tooltip)]
                     ; what about class
                     [anchor ctx props @(r/track prompt link-ref ?label)])]
            ))))))

(defn ^:export link "Relation level link renderer. Works in forms and lists but not tables."
  [rel relative-path ctx & [?label props]]                  ; path should be optional, for disambiguation only. Naked can be hard-linked in markdown?
  (let [ctx (context/focus ctx relative-path)
        link-ref (r/track link/rel->link rel ctx)]
    [ui-from-link link-ref ctx props ?label]))

(defn ^:export browse "Relation level browse. Works in forms and lists but not tables."
  [rel relative-path ctx & [?user-renderer props]]          ; path should be optional, for disambiguation only. Naked can be hard-linked in markdown?
  (let [ctx (context/focus ctx relative-path)
        link-ref (r/track link/rel->link rel ctx)
        props (if ?user-renderer
                (assoc props :user-renderer ?user-renderer)
                props)]
    [ui-from-link link-ref ctx props]))

(defn form-field "Form fields are label AND value. Table fields are label OR value."
  [hyper-control relative-path ctx ?f props]                ; ?f :: (val props ctx) => DOM
  (let [state (r/atom {:hyperfiddle.ui.form/magic-new-a nil})]
    (fn [hyper-control relative-path ctx ?f props]
      (let [ctx (assoc ctx :hyperfiddle.ui.form/state state)
            ; we want the wrapper div to have the :body styles, so careful not to pollute the head ctx with :body
            body-ctx (context/focus ctx (cons :body relative-path))
            head-ctx (context/focus ctx (cons :head relative-path))
            props (update props :class css (semantic-css body-ctx))]
        ; It is the driver-fn's job to elide this field if it will be empty
        [:div {:class (css "field" (:class props))
               :style {:border-color (border-color body-ctx)}}
         ^{:key :form-head}
         [(or (:label-fn props) (hyper-control head-ctx)) nil props head-ctx]
         ^{:key :form-body}
         [:div
          [(or ?f (hyper-control body-ctx)) @(:hypercrud.browser/data body-ctx) props body-ctx]]]))))

(defn table-field "Form fields are label AND value. Table fields are label OR value."
  [hyper-control relative-path ctx ?f props]                ; ?f :: (val props ctx) => DOM
  (let [head-or-body (last (:hypercrud.browser/path ctx))   ; this is ugly and not uniform with form-field
        ctx (context/focus ctx relative-path)
        props (update props :class css (semantic-css ctx))]
    (case head-or-body
      :head [:th {:class (css "field" (:class props)
                              (when (sort/sortable? ctx) "sortable") ; hoist
                              (some-> (sort/sort-direction relative-path ctx) name)) ; hoist
                  :style {:background-color (border-color ctx)}
                  :on-click (r/partial sort/toggle-sort! relative-path ctx)}
             [(or (:label-fn props) (hyper-control ctx)) nil props ctx]]
      :body [:td {:class (css "field" (:class props) "truncate")
                  :style {:border-color (when (:hypercrud.browser/source-symbol ctx) (border-color ctx))}}
             [(or ?f (hyper-control ctx)) @(:hypercrud.browser/data ctx) props ctx]])))

; (defmulti field ::layout)
(defn ^:export field "Works in a form or table context. Draws label and/or value."
  [relative-path ctx & [?f props]]                          ; ?f :: (ref, props, ctx) => DOM
  (let [is-magic-new (= '* (last relative-path))]
    (case (:hyperfiddle.ui/layout ctx)
      :hyperfiddle.ui.layout/table (when-not is-magic-new
                                     ^{:key (str relative-path)}
                                     [table-field hyper-control relative-path ctx ?f props])
      (let [magic-new-key (when is-magic-new
                            (let [ctx (context/focus ctx (cons :body relative-path))]
                              ; guard against crashes for nil data
                              (hash (some->> ctx :hypercrud.browser/parent :hypercrud.browser/data (r/fmap keys) deref))))]
        ^{:key (str relative-path magic-new-key #_"reset magic new state")}
        [form-field hyper-control relative-path ctx ?f props]))))

(defn ^:export table "Semantic table"
  [form ctx & [props]]
  (let [sort-col (r/atom nil)
        sort (fn [v] (hyperfiddle.ui.sort/sort-fn v sort-col))]
    (fn [form ctx & [props]]
      (let [ctx (assoc ctx ::sort/sort-col sort-col
                           ::layout :hyperfiddle.ui.layout/table)]
        [:table (update props :class css "ui-table" "unp" (semantic-css ctx))
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

(letfn [(field-with-props [props relative-path ctx] (field relative-path ctx nil props))]
  (defn ^:export result "Default result renderer. Invoked as fn, returns seq-hiccup, hiccup or
nil. call site must wrap with a Reagent component"
    [ctx & [props]]
    ; focus should probably get called here. What about the request side?
    (fragment
      (hint ctx)
      (case @(r/fmap ::field/cardinality (:hypercrud.browser/field ctx))
        :db.cardinality/one (let [ctx (assoc ctx ::layout :hyperfiddle.ui.layout/block)
                                  key (-> @(r/fmap data/row-keyfn (:hypercrud.browser/data ctx)) str keyword)]
                              (apply fragment key (data/form (r/partial field-with-props props) ctx)))
        :db.cardinality/many [table (r/partial data/form (r/partial field-with-props props)) ctx props]
        ; blank fiddles
        nil))))

(def ^:dynamic markdown)                                    ; this should be hf-contrib or something

(def ^:export fiddle (-build-fiddle))

(defn ^:export fiddle-xray [ctx class]
  (let [{:keys [:hypercrud.browser/fiddle]} ctx]
    [:div {:class class}
     [:h3 (pr-str (:route ctx)) #_(some-> @fiddle :fiddle/ident str)]
     (result ctx)]))

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
