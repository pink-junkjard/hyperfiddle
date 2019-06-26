(ns hyperfiddle.ui.select$                                  ; Namespace clashes with var hyperfiddle.ui/select
  (:require
    [cats.core :as cats :refer [mlet return]]
    [cats.monad.either :as either :refer [left right branch]]
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [unqualify]]
    [contrib.eval]
    [contrib.reactive :as r]
    [contrib.reader]
    [contrib.string :refer [blank->nil]]
    [cuerdas.core :as str]
    [goog.object :as object]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hyperfiddle.api :as hf]
    [hyperfiddle.data :as data]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.error :as ui-error]
    [hyperfiddle.ui.popover :as popover]
    [hyperfiddle.ui.stale :as stale]
    [hyperfiddle.ui.util :refer [with-entity-change! writable-entity?]]
    [taoensso.timbre :as timbre]))

(defn ident->label [v]
  (cond
    ; A sensible default for userland whose idents usually share a long namespace.
    (instance? cljs.core/Keyword v) (name v)
    (and (vector? v) (= 1 (count v))) (str (first v))
    :else (str v)))

;(defn option-label-default' [row ctx]                                ; typechecks with keyword
;  ; spread-rows is external, f.
;  (->>
;    (for [[_ ctx] (hypercrud.browser.context/spread-fiddle ctx)
;          [_ ctx] (hypercrud.browser.context/spread-elements ctx)]
;      (let [[_ _ v] @(:hypercrud.browser/eav ctx)]
;        (condp = (type @(:hypercrud.browser/element ctx))
;          Variable [v]
;          Aggregate [v]
;          Pull (for [[_ ctx] (hypercrud.browser.context/spread-attributes ctx)]
;                 (let [[_ _ v] @(:hypercrud.browser/eav ctx)]
;                   (ident->label v))))))
;    (mapcat identity)
;    (remove nil?)
;    (interpose ", ")
;    (apply str)))

;(defn option-label-default [row ctx]
;  ; row is FindRel or FindCol
;  (or
;    (some-> (hf/row-key ctx row) ident->label)
;    (clojure.string/join " " (vals row))))

(defn wrap-change-for-select [hf-change!]
  (fn [e]
    (-> (some->> (.-target.value e)
                 blank->nil
                 contrib.reader/read-edn-string+
                 ; todo why handle this exception? just throw and call it a day
                 ; instead of terminating on error, user now transacts a retract
                 (unwrap #(timbre/warn %)))
        hf-change!)))

(declare options-value-bridge+)
(declare select-error-cmp)

(defn select-html [_ ctx props]                             ; element, etc
  (branch
    (options-value-bridge+ (::value-ctx props) props)
    (fn [err]
      [select-error-cmp err])
    (fn [[select-props option-props]]
      ; hack in the selected value if we don't have options hydrated?
      ; Can't, since we only have the #DbId hydrated, and it gets complicated with relaton vs entity etc
      ; This is possible if the value matches the row-key (qfind) of options query
      (let [is-no-options (empty? (hf/data ctx))
            select-props (-> select-props
                             (assoc :value (str (hf/v (::value-ctx props)))) ; serialize v to string for dom roundtrip
                             (update :on-change wrap-change-for-select)
                             (dissoc :disabled)             ; Use :read-only instead to allow click to expand options
                             (update :read-only #(or % (:disabled select-props) is-no-options))
                             (update :class #(str % (if (:disabled option-props) " disabled"))))]
        [:select.ui (select-keys select-props [:value :class :style :on-change :read-only :on-click]) ;(dissoc props :option-label)
         ; .ui is because options are an iframe and need the pink box
         (conj
           (->> (hf/data ctx)
                (mapv (juxt #((:option-value select-props) % ctx) ; is lookup ref good yet?
                            #((:option-label select-props) % ctx)))
                (sort-by second)
                (map (fn [[id label]]
                       [:option (assoc option-props :key (str id) :value (str id)) label])))
           [:option (assoc option-props :key :blank :value "") "--"])])
      )))

(defn select-error-cmp [msg]
  [:span msg])

(defn compute-disabled [ctx props]
  (or (boolean (:disabled props))
      (boolean (:read-only props))                          ; legacy
      (nil? (hf/e ctx))))

(defn options-value-bridge+ [anchor-ctx #_target-ctx props] ; no longer need target-ctx because it has to work without options hydrated
  ; if select is qfind-level, is value tupled according to options qfind?
  ; if select is pull-level, is options qfind untupled?
  ; We must compare both ctxs to decide this.
  (let [option-element (:option-element props 0)
        select-props {:option-element option-element
                      :option-value (let []
                                      (fn [val ctx]
                                        ; in the case of tupled options, the userland view props must
                                        ; indicate which element is the entity has an identity which matches the anchor eav.
                                        (condp some [(unqualify (contrib.datomic/parser-type @(:hypercrud.browser/qfind ctx)))]
                                          #{:find-coll :find-scalar} (hf/row-key ctx val)
                                          #{:find-rel :find-tuple} (get (hf/row-key ctx val) option-element))))
                      :option-label (fn [val]
                                      (let [option-label (contrib.eval/ensure-fn (:option-label props pr-str))]
                                        (option-label val)))}
        ; The pulled v is always the select value, options must align
        ; There is always an attribute here because all widgets are attribute-centric
        value (hf/v anchor-ctx)]
    (cond
      ; Select options renderer is an eav oriented control like all controls, thus the
      ; anchor is always a pull context, never a qfind context. To see this is true, consider that you can't
      ; pick a tuple then write it into a database. You can only write through entities.
      (hf/qfind-level? anchor-ctx)
      (left (str "No attribute in scope. eav: " (hf/eav anchor-ctx)))

      :else
      (let [select-props (merge {:value value               ; typeahead does not use this
                                 :on-change (with-entity-change! anchor-ctx)}
                                select-props
                                props)
            option-props {:disabled (compute-disabled anchor-ctx select-props)}]
        (right [select-props option-props])))))

(defn- options-values [options-ctx select-props]
  (condp some [(unqualify (contrib.datomic/parser-type (hf/qfind options-ctx)))]
    #{:find-coll :find-scalar} (hf/data options-ctx)
    #{:find-rel :find-tuple} (mapv #(get % (:option-element select-props 0))
                                   (hf/data options-ctx))))

(defn- truncated-options [n]
  ; todo this destroys table cells
  #_(when (= base/browser-query-limit n)
      [:div.alert.alert-warning (str/format "Warning: Options resultset has been truncated to %s records. Please add additional filters" base/browser-query-limit)]))

(defn typeahead-html [_ options-ctx props]                  ; element, etc
  (branch
    (options-value-bridge+ (::value-ctx props) props)
    (fn [err]
      [select-error-cmp err])
    (fn [[select-props option-props]]
      ; hack in the selected value if we don't have options hydrated?
      ; Can't, since we only have the #DbId hydrated, and it gets complicated with relaton vs entity etc
      ; This is possible if the value matches the row-key (qfind) of options query
      (let [is-no-options (empty? (hf/data options-ctx))
            select-props (-> select-props
                             (dissoc :disabled)             ; Use :read-only instead to allow click to expand options
                             (update :read-only #(or % (:disabled select-props) is-no-options))
                             (update :class #(str % (if (:disabled option-props) " disabled"))))

            ?v (hf/data (::value-ctx props))

            ; Need schema set and in multicolor case you need to know which element
            option-records-untupled (options-values options-ctx select-props)


            ; Convoluted context manipulation due to https://github.com/hyperfiddle/hyperfiddle/issues/949
            ; The above untupling logic should be done by hyperfiddle
            options-ctx (hf/browse-element options-ctx (:option-element select-props 0))
            #_#_option-records-untupled' (hf/data options-ctx)
            #_#__ (js/console.log option-records-untupled option-records-untupled')]
        [:<>
         [truncated-options (count option-records-untupled)]
         (reagent.core/create-element
           js/ReactBootstrapTypeahead.Typeahead
           #js {"labelKey" (fn [record]
                             ; Must return string otherwise "invariant undefined"; you can pr-str from userland
                             (str ((:option-label select-props) record)))
                "placeholder" (:placeholder select-props)
                ; widget requires the option records, not ids
                "options" (->> option-records-untupled (sort-by (:option-label select-props)) to-array)
                "onChange" (fn [jrecord]
                             ; foreign lib state is js array, single select is lifted into List like multi-select
                             ; unselected is []
                             (let [[?record] (array-seq jrecord)
                                   ?v (hf/id options-ctx ?record)]
                               ((:on-change select-props) ?v)))
                ; V might not be in options - widget accounts for this by taking a selected record rather than identity
                ; V might have different keys than options - as long as :option-label works, it doesn't matter
                "selected" (if ?v #js [?v] #js [])

                "highlightOnlyResult" true                  ; this helps avoid

                ; Rendering strategy that works in tables
                ; http://hyperfiddle.hyperfiddle.site/:hyperfiddle.ide!edit/(:fiddle!ident,:hyperfiddle!ide)
                "bodyContainer" true
                "align" "left"})]))))

(defn- props->async-typeahead [props]
  (let [j-props (apply js-obj (apply concat props))]
    (reagent.core/create-element js/ReactBootstrapTypeahead.AsyncTypeahead j-props)))

(defn- select-needle-typeahead [{rt :peer branch :branch :as branched-unrouted-ctx} value
                                initial-options
                                ; todo these props need cleaned up, shouldn't be adapting them over and over again
                                select-props options-props]
  (let [common-props {
                      #_#_"delay" 200
                      "isLoading" (runtime/branch-is-loading? rt branch)
                      #_#_"minLength" 2
                      "onSearch" (fn [s]
                                   (-> (context/build-route+ [s] branched-unrouted-ctx)
                                       (either/branch
                                         (fn [e] (runtime/set-branch-error rt branch e))
                                         (fn [route]
                                           (when-not (runtime/branch-exists? rt branch) ; todo this is crap
                                             (runtime/dispatch! rt [:create-partition branch]))
                                           (runtime/set-route rt branch route)))))

                      ; Must return string otherwise "invariant undefined"; you can pr-str from userland
                      "labelKey" (comp str (:option-label select-props))
                      "placeholder" (:placeholder select-props)
                      ; V might not be in options - widget accounts for this by taking a selected record rather than identity
                      ; V might have different keys than options - as long as :option-label works, it doesn't matter
                      "selected" (if value #js [value] #js [])

                      "highlightOnlyResult" true            ; this helps avoid

                      ; Rendering strategy that works in tables
                      ; http://hyperfiddle.hyperfiddle.site/:hyperfiddle.ide!edit/(:fiddle!ident,:hyperfiddle!ide)
                      "bodyContainer" true
                      "align" "left"

                      #_#_"promptText" "Type to search..."
                      #_#_"searchText" "Searching..."
                      "useCache" false}
        adapt-options (fn [options] (to-array (sort-by (:option-label select-props) options)))]
    [:div
     [stale/loading (r/track runtime/branch-is-loading? rt branch)
      (if-let [route (runtime/get-route rt branch)]         ; no route means no data yet
        (base/browse-route+ branched-unrouted-ctx route)
        (either/right nil))
      (fn [e]
        ; even if browsing fails, the user needs a chance to alter their search, so just add an ugly error message
        [:<>
         [select-error-cmp (or (ex-message e) (str e))]     ; should use error-comp, wrong ctx in scope though
         [truncated-options (count initial-options)]
         ^{:key :async-typeahead}
         [props->async-typeahead
          (assoc common-props
            "options" (adapt-options initial-options)
            "onChange" (fn [jrecord]
                         ; foreign lib state is js array, single select is lifted into List like multi-select
                         ; unselected is []
                         (let [[?record] (array-seq jrecord)]
                           (if (nil? ?record)               ; not sure how this can be anything but nil
                             ((:on-change select-props) nil)
                             (runtime/set-branch-error rt branch (ex-info "Record selected when no records hydrated"
                                                                          {:record (pr-str ?record)}))))))]])
      (fn [?options-ctx]
        (if-let [options-ctx ?options-ctx]
          [:<>
           [truncated-options @(r/fmap count (:hypercrud.browser/result options-ctx))]
           ^{:key :async-typeahead}
           [props->async-typeahead
            (assoc common-props
              ; widget requires the option records, not ids
              "options" (->> (condp some [(unqualify (contrib.datomic/parser-type (hf/qfind options-ctx)))]
                               #{:find-coll :find-scalar} (hf/data options-ctx)
                               #{:find-rel :find-tuple} (mapv #(get % (:option-element select-props 0))
                                                              (hf/data options-ctx)))
                             adapt-options)
              "onChange" (fn [jrecord]
                           ; foreign lib state is js array, single select is lifted into List like multi-select
                           ; unselected is []
                           (let [[?record] (array-seq jrecord)
                                 element-ctx (hf/browse-element options-ctx (:option-element select-props 0))
                                 ?id (hf/id element-ctx ?record)]
                             ((:on-change select-props) ?id))))]]
          [:<>
           [truncated-options (count initial-options)]
           ^{:key :async-typeahead}
           [props->async-typeahead
            (assoc common-props
              "options" (adapt-options initial-options)
              "onChange" (fn [jrecord]
                           ; foreign lib state is js array, single select is lifted into List like multi-select
                           ; unselected is []
                           (let [[?record] (array-seq jrecord)]
                             (if (nil? ?record)             ; not sure how this can be anything but nil
                               ((:on-change select-props) nil)
                               (runtime/set-branch-error rt branch (ex-info "Record selected when no records hydrated"
                                                                            {:record (pr-str ?record)}))))))]]))]]))

(defn ^:export select-needle
  ([_ ctx props] (select-needle ctx props))
  ([ctx props]
   (assert (:options props) "select: :options prop is required")
   ; any error in this mlet is fatal, don't bother rendering the typeahead
   (-> (mlet [link-ref (data/select+ ctx (keyword (:options props)))
              link-ctx (context/refocus-to-link+ ctx link-ref)]
         (-> #_(context/build-route-and-occlude+ link-ctx link-ref) ; todo too many errors are being caught in build-route-and-occlude+ that are completely fatal (e.g. formula eval errors); need to direct more control over route building here
           (context/build-route+ [] link-ctx)               ; hand craft args, independent iframes cannot be represented in the links model
           (cats/bind (fn [route] (base/browse-route+ link-ctx route)))
           (either/branch
             (fn [e]
               ; if this failed we don't care, either not an iframe or the route is likely incomplete, just show no initial options
               (mlet [:let [relative-branch-id (popover/build-child-branch-relative-id ctx link-ref (context/eav ctx))]
                      branched-unrouted-ctx (context/branch+ link-ctx relative-branch-id)
                      [select-props options-props] (options-value-bridge+ ctx props)]
                 (return [select-needle-typeahead branched-unrouted-ctx (hf/data ctx) [] select-props options-props])))
             (fn [options-ctx]
               (cond
                 ; hybrid, use the existing iframe to render initial options, but use the needle to filter on server
                 @(r/fmap-> link-ref :link/fiddle :fiddle/query-needle some?)
                 (mlet [:let [relative-branch-id (popover/build-child-branch-relative-id ctx link-ref (context/eav ctx))]
                        branched-unrouted-ctx (context/branch+ link-ctx relative-branch-id)
                        [select-props options-props] (options-value-bridge+ ctx props)]
                   (return
                     [select-needle-typeahead branched-unrouted-ctx (hf/data ctx)
                      (options-values options-ctx select-props)
                      select-props options-props]))

                 ; iframe (data is local), complete-route, and no needle
                 ; just use client side filtering on the intial options
                 :else (return [typeahead-html nil options-ctx (assoc props ::value-ctx ctx)]))
               ))))
       (either/branch
         (fn [e] [(ui-error/error-comp ctx) e])
         identity))))

(defn ^:export typeahead [ctx props]
  [select-needle ctx props])

(defn ^:export select
  ([ctx props] [select-needle ctx props])
  ([_ ctx props] (select ctx props)))
