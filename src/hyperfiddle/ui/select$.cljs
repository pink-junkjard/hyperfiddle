(ns hyperfiddle.ui.select$                                  ; Namespace clashes with var hyperfiddle.ui/select
  (:require
    [cats.core :refer [mlet return]]
    [cats.monad.either :refer [left right branch]]
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [unqualify]]
    [contrib.eval]
    [contrib.reader]
    [contrib.string :refer [blank->nil]]
    [hyperfiddle.api :as hf]
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

(defn option-label-default [row ctx]
  ; row is FindRel or FindCol
  (or
    (some-> (hf/row-key ctx row) ident->label)
    (clojure.string/join " " (vals row))))

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
                      :option-label (fn [val ctx]
                                      (let [option-label (contrib.eval/ensure-fn (:option-label props option-label-default))]
                                        (option-label val ctx)))}
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

(declare typeahead)

(defn ^:export select
  ([ctx props]
   (select nil ctx props))
  ([_ ctx props]
   #_[typeahead ctx props]
   {:pre [ctx]}
   (assert (:options props) "select: :options prop is required")
    ; http://hyperfiddle.hyperfiddle.net/:database!options-list/
    ; http://hyperfiddle.hyperfiddle.site/:hyperfiddle.ide!domain/~entity('$domains',(:domain!ident,'hyperfiddle'))
   [hyperfiddle.ui/link (keyword (:options props))
    #_#_ctx (assoc ctx :hypercrud.ui/display-mode (r/pure :hypercrud.browser.browser-ui/user))
    ctx nil
    (assoc props
      :user-renderer select-html
      ::value-ctx ctx)]))

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
            option-records-untupled
            (condp some [(unqualify (contrib.datomic/parser-type (hf/qfind options-ctx)))]
              #{:find-coll :find-scalar} (hf/data options-ctx)
              #{:find-rel :find-tuple} (mapv #(get % (:option-element select-props 0))
                                             (hf/data options-ctx)))

            ; Convoluted context manipulation due to https://github.com/hyperfiddle/hyperfiddle/issues/949
            ; The above untupling logic should be done by hyperfiddle
            options-ctx (hf/browse-element options-ctx (:option-element select-props 0))
            #_#_option-records-untupled' (hf/data options-ctx)
            #_#__ (js/console.log option-records-untupled option-records-untupled')]

        (reagent.core/create-element
          js/ReactBootstrapTypeahead.Typeahead
          #js {"labelKey" (fn [record]
                            ((:option-label select-props) record options-ctx))
               "placeholder" (:placeholder select-props)
               "options" (to-array option-records-untupled) ; widget requires the option records, not ids
               "onChange" (fn [jrecord]
                            ; foreign lib state is js array, single select is lifted into List like multi-select
                            ; unselected is []
                            (let [[?record] (array-seq jrecord)
                                  ?v (hf/id options-ctx ?record)]
                              ((:on-change select-props) ?v)))
               ; V might not be in options - widget accounts for this by taking a selected record rather than identity
               ; V might have different keys than options - as long as :option-label works, it doesn't matter
               "selected" (if ?v #js [?v] #js [])

               ; Rendering strategy that works in tables
               ; http://hyperfiddle.hyperfiddle.site/:hyperfiddle.ide!edit/(:fiddle!ident,:hyperfiddle!ide)
               "bodyContainer" true
               "align" "left"})))))

(defn ^:export typeahead
  [ctx props]
  (assert (:options props) "select: :options prop is required")
  [hyperfiddle.ui/link (keyword (:options props)) ctx nil
   (assoc props
     :user-renderer typeahead-html
     ::value-ctx ctx)])
