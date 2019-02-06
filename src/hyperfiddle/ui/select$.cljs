(ns hyperfiddle.ui.select$                                  ; Namespace clashes with var hyperfiddle.ui/select
  (:require
    [cats.core :refer [mlet return]]
    [cats.monad.either :as either]
    [contrib.ct :refer [unwrap]]
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
                         (update :class #(str % (if (:disabled option-props) " disabled"))))
        label-fn (contrib.eval/ensure-fn (:option-label select-props option-label-default))]
    [:select.ui (select-keys select-props [:value :class :style :on-change :read-only :on-click]) ;(dissoc props :option-label)
     ; .ui is because options are an iframe and need the pink box
     (conj
       (->> (hypercrud.browser.context/data ctx)
            (mapv (juxt #(context/row-key ctx %) #(label-fn % ctx))) ; is lookup ref good yet?
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

(defn options-value-bridge [select-view
                            anchor-props anchor-ctx
                            target-val target-ctx target-props]
  ; if select is qfind-level, is value tupled according to options qfind?
  ; if select is pull-level, is options qfind untupled?
  ; We must compare both ctxs to decide this.
  (let [options (->> (hypercrud.browser.context/data target-ctx)
                     (map #(context/row-key target-ctx %))) ; no fallback v, that can be a failure case if records aren't distinct.

        target-qfind-type (some-> (:hypercrud.browser/qfind target-ctx) deref type)]
    (cond
      ; Select options renderer is an eav oriented control like all controls, thus the
      ; anchor is always a pull context, never a qfind context. To see this is true, consider that you can't
      ; pick a tuple then write it into a database. You can only write through entities.
      (context/qfind-level? anchor-ctx)
      [select-error-cmp (str "Select requires attribute ctx, eav: " (context/eav anchor-ctx))]

      (and (:value anchor-props) (not (contains? (set options) (:value anchor-props))))
      [select-error-cmp (str "Select value: " (:value anchor-props) " not seen in options")]

      (#{FindColl FindScalar} target-qfind-type)
      (let [options-props (into anchor-props (select-keys target-props [:on-click]))
            option-props {:disabled (compute-disabled anchor-ctx anchor-props)}]
        [select-view options-props option-props target-ctx])

      ; Because of attribute-orientation, anchor-value is never tupled. Therefore the options-qfind must
      ; always be untupled (FindColl). FindRel options are never valid, unless FindRel-1 can
      ; theoretically collapse here. (See comment in row-key)
      (#{FindRel FindTuple} target-qfind-type)
      [select-error-cmp (str "Options must be FindColl, got: " (name (contrib.datomic/parser-types target-qfind-type)))]

      ; A possible valid FindRel-N is when first element is Pull and remaining are aggregates, meaning
      ; the first element is sufficient identity http://hyperfiddle.hyperfiddle.net/:database!options-list/
      )))

(defn select "This arity should take a selector string (class) instead of Right[Reaction[Link]],
  blocked on removing path backdoor"
  [val ctx props]
  {:pre [ctx]}
  (assert (:options props) "select: :options prop is required")
  (-> (mlet [options-ref (data/select+ ctx (keyword (:options props)))] ; coerce somewhere else tho
        (return
          (let [default-props {:on-change (with-entity-change! ctx)}
                props (-> (merge default-props props)
                          (assoc :value (let [[_ _ v] @(:hypercrud.browser/eav ctx)]
                                          v #_(context/underlying-tempid ctx v))))
                props (-> (select-keys props [:class])
                          (assoc :user-renderer (r/partial options-value-bridge select-html props ctx)))
                ctx (assoc ctx
                      :hypercrud.ui/display-mode (r/track identity :hypercrud.browser.browser-ui/user))]
            [hyperfiddle.ui/ui-from-link options-ref ctx props])))
      (either/branch select-error-cmp identity)))
