(ns hyperfiddle.ui.select$                                  ; Namespace clashes with var hyperfiddle.ui/select
  (:require
    [cats.core :refer [mlet return]]
    [cats.monad.either :as either]
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [unqualify]]
    [contrib.eval]
    [contrib.reactive :as r]
    [contrib.reader]
    [contrib.string :refer [blank->nil]]
    [datascript.parser :refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]]
    [hypercrud.browser.context :as context]
    [hyperfiddle.data :as data]
    [hyperfiddle.ui.util :refer [with-entity-change! writable-entity?]]
    [taoensso.timbre :as timbre]
    [cljs.spec.alpha :as s]))

(defn ident->label [v]
  (if (instance? cljs.core/Keyword v)
    ; A sensible default for userland whose idents usually share a long namespace.
    (name v)
    (str v)))

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
    (some-> (hypercrud.browser.context/row-key ctx row) ident->label)
    (clojure.string/join " " (vals row))))

(defn select-html [select-props option-props ctx]           ; element, etc
  ; hack in the selected value if we don't have options hydrated?
  ; Can't, since we only have the #DbId hydrated, and it gets complicated with relaton vs entity etc
  ; This is possible if the value matches the row-key (qfind) of options query
  (let [is-no-options (empty? (hypercrud.browser.context/data ctx))
        select-props (-> select-props
                         (update :value str)
                         (update :on-change (fn [on-change]
                                              (fn [e]
                                                (-> (some->> (.-target.value e)
                                                             blank->nil
                                                             contrib.reader/read-edn-string+
                                                             ; todo why handle this exception? just throw and call it a day
                                                             ; instead of terminating on error, user now transacts a retract
                                                             (unwrap #(timbre/warn %)))
                                                    on-change))))
                         (dissoc :disabled)                 ; Use :read-only instead to allow click to expand options
                         (update :read-only #(or % (:disabled select-props) is-no-options))
                         (update :class #(str % (if (:disabled option-props) " disabled"))))]
    [:select.ui (select-keys select-props [:value :class :style :on-change :read-only :on-click]) ;(dissoc props :option-label)
     ; .ui is because options are an iframe and need the pink box
     (conj
       (->> (context/data ctx)
            (mapv (juxt #((:option-value select-props) % ctx)     ; is lookup ref good yet?
                        #((:option-label select-props) % ctx)))
            (sort-by second)
            (map (fn [[id label]]
                   [:option (assoc option-props :key (str id) :value (str id)) label])))
       [:option (assoc option-props :key :blank :value "") "--"])]))

(defn select-error-cmp [msg]
  [:span msg])

(defn compute-disabled [ctx props]
  (let [[e _ _] (context/eav ctx)]
    (or (boolean (:disabled props))
        (boolean (:read-only props))                        ; legacy
        (nil? e))))

(defn options-value-bridge [select-view select-props anchor-ctx
                            target-val target-ctx target-props]
  ; if select is qfind-level, is value tupled according to options qfind?
  ; if select is pull-level, is options qfind untupled?
  ; We must compare both ctxs to decide this.
  (let [value (context/v anchor-ctx) #_((:option-value select-props) (context/data anchor-ctx) anchor-ctx)
        options (->> (context/data target-ctx)
                     (map #((:option-value select-props) % target-ctx)))]
    (cond
      ; Select options renderer is an eav oriented control like all controls, thus the
      ; anchor is always a pull context, never a qfind context. To see this is true, consider that you can't
      ; pick a tuple then write it into a database. You can only write through entities.
      (context/qfind-level? anchor-ctx)
      [select-error-cmp (str "No attribute in scope. eav: " (context/eav anchor-ctx))]

      (and value (not (contains? (set options) value)))
      [select-error-cmp (str "Value not seen in options: " value)]

      :else
      (let [select-props (merge {:value value
                                 :on-change (with-entity-change! anchor-ctx)}
                                select-props
                                (select-keys target-props [:on-click]))
            options-props {:disabled (compute-disabled anchor-ctx select-props)}]
        [select-view select-props options-props target-ctx]))))

(defn select "This arity should take a selector string (class) instead of Right[Reaction[Link]],
  blocked on removing path backdoor"
  [val ctx props]
  {:pre [ctx]}
  (assert (:options props) "select: :options prop is required")
  (-> (mlet [options-ref (data/select+ ctx (keyword (:options props)))] ; coerce somewhere else tho
        (return
          ; http://hyperfiddle.hyperfiddle.net/:database!options-list/
          ; http://hyperfiddle.hyperfiddle.site/:hyperfiddle.ide!domain/~entity('$domains',(:domain!ident,'hyperfiddle'))
          (let [select-props {:option-value (let [option-element (contrib.eval/ensure-fn (:option-element props first))]
                                              (fn [val ctx]
                                                ; in the case of tupled options, the userland view props must
                                                ; indicate which element is the entity has an identity which matches the anchor eav.
                                                (condp some [(unqualify (contrib.datomic/parser-type @(:hypercrud.browser/qfind ctx)))]
                                                  #{:find-coll :find-scalar} (context/row-key ctx val)
                                                  #{:find-rel :find-tuple} (option-element (context/row-key ctx val)))))
                              :option-label (fn [val ctx]
                                              (let [option-label (contrib.eval/ensure-fn (:option-label props option-label-default))]
                                                (option-label val ctx)))}
                anchor-props {:class (:class props)
                              :user-renderer (r/partial options-value-bridge select-html select-props ctx)}
                ctx (assoc ctx :hypercrud.ui/display-mode (r/track identity :hypercrud.browser.browser-ui/user))]
            [hyperfiddle.ui/ui-from-link options-ref ctx anchor-props])))
      (either/branch select-error-cmp identity)))
