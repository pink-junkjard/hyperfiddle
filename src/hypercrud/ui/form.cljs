(ns hypercrud.ui.form
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [cats.monad.exception :as exception]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.browser.context :as context]
            [hypercrud.compile.eval :refer [eval-str']]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.renderer :as renderer]
            [hypercrud.ui.widget :as widget]
            [hypercrud.util.monad :refer [exception->either]]
            [reagent.core :as r]))

(defn control [maybe-field anchors param-ctx]
  (let [props (form-util/build-props maybe-field anchors param-ctx)]
    (if (renderer/user-renderer param-ctx)
      (renderer/user-render maybe-field anchors props param-ctx)
      [auto-control maybe-field anchors props param-ctx])))

(defn field [control maybe-field anchors param-ctx]
  (let [
        ;; Check visibility user fn. Error states: eval error, apply error.
        ;maybe-visible' (if-let [user-fn-str (:field/visible? maybe-field)]
        ;                 (if-not (empty? user-fn-str)
        ;                   (mlet [user-fn (eval-str' user-fn-str)]
        ;                     ; predicate gets whole entity as arg 1, for keyword syntax sugar
        ;                     (-> (exception/try-on (user-fn (:entity param-ctx) param-ctx))
        ;                         exception->either))))
        ;hidden (not (or (some-> maybe-visible'
        ;                        (cats/mplus (either/right true))
        ;                        (cats/extract))
        ;                true))
        ;
        ;; Draw error instead of control.
        ;control (if (either/left? maybe-visible')
        ;          [:pre (js/pprint-str (cats/extract maybe-visible'))])
        ]
    [:div.field {:style {:border-color (connection-color/connection-color (:color param-ctx))}}
     (let [[anchors] (as-> anchors $
                           (remove :anchor/repeating? $)      ; because we're in the label
                           (widget/process-option-anchors $ param-ctx))]
       [:div.hc-label
        [:label (form-util/field-label maybe-field param-ctx)]
        [:div.anchors
         (widget/render-anchors (->> anchors (remove :anchor/render-inline?)) param-ctx)
         (widget/render-inline-anchors (->> anchors (filter :anchor/render-inline?)) param-ctx)]])
     (control param-ctx)]))

(defn new-field [entity param-ctx]
  (let [attr-ident (r/atom nil)]
    (fn [entity param-ctx]
      [:div.field {:style {:border-color (connection-color/connection-color (-> entity :db/id :conn-id))}}
       [:div.hc-label
        [:label
         (let [on-change! #(reset! attr-ident %)]
           [input/keyword-input* @attr-ident on-change!])]]
       (let [on-change! #(let [tx [[:db/add (:db/id entity) @attr-ident %]]]
                           ; todo cardinality many
                           ((:user-with! param-ctx) tx))
             props (if (nil? @attr-ident) {:read-only true})]
         [input/edn-input* nil on-change! props])])))

(def always-read-only (constantly true))

(defn form [relation colspec anchors param-ctx]
  ; all anchors need a find-element at least, because it has a connection affinity.
  (let [param-ctx (-> (context/relation param-ctx relation)
                      (assoc :layout (:layout param-ctx :block)))
        anchors (widget/process-popover-anchors anchors param-ctx)
        anchors-lookup (->> (remove :anchor/attribute anchors)
                            (group-by (comp :find-element/name :anchor/find-element)))
        fields (->> (partition 4 colspec)
                    (group-by (fn [[dbval fe attr maybe-field]] fe))
                    (mapcat
                      (fn [[fe colspec]]
                        (let [fe-name (-> fe :find-element/name)
                              form-anchors (get anchors-lookup fe-name)
                              entity-new-anchors (->> form-anchors (remove :anchor/repeating?))
                              entity-anchors (->> form-anchors (filter :anchor/repeating?))
                              db (ffirst colspec)
                              param-ctx (context/find-element param-ctx db fe)]
                          (concat
                            ; don't put entity in scope because it messes up formulas which have to be deterministic with request side.
                            (widget/render-anchors (remove :anchor/render-inline? entity-new-anchors) param-ctx)
                            (let [entity (get relation fe-name)
                                  param-ctx (context/entity param-ctx entity)]
                              #_(assert entity "i think this is true now")
                              (concat
                                (widget/render-anchors (remove :anchor/render-inline? entity-anchors) param-ctx)
                                (->> colspec
                                     (mapv (fn [[db fe attr maybe-field]]
                                             (let [ident (-> attr :attribute/ident)
                                                   param-ctx (as-> (context/attribute param-ctx attr) $
                                                                   (context/value $ (get entity ident))
                                                                   (if (= ident :db/id) (assoc $ :read-only always-read-only) $))
                                                   field (case (:display-mode param-ctx) :xray field :user (get param-ctx :field field))
                                                   control (case (:display-mode param-ctx) :xray control :user (get param-ctx :control control))
                                                   anchors (filter #(= (-> param-ctx :attribute :db/id) (some-> % :anchor/attribute :db/id)) anchors)]
                                               ; What is the user-field allowed to change? The param-ctx. Can it change links or anchors? no.
                                               ^{:key (str ident)}
                                               [field #(control maybe-field anchors %) maybe-field anchors param-ctx]))))
                                (widget/render-inline-anchors (filter :anchor/render-inline? entity-anchors) param-ctx)))
                            (widget/render-inline-anchors (filter :anchor/render-inline? entity-new-anchors) param-ctx))))))
        not-splat? (and (not (empty? colspec))
                        (->> (partition 4 colspec)
                             (mapv (fn [[conn fe-name attr maybe-field]] maybe-field))
                             (every? #(not= nil %))))
        magic-new-field (if-not not-splat?
                          ; can we assert entity? No, bc we could model a link to a single relation without a form.
                          (if-let [entity (get relation "entity")] ; makes sense only for entity links, not query links as entity.
                            (let [[db fe _ _] (take 4 colspec)
                                  param-ctx (context/find-element param-ctx db fe)]
                              ^{:key (hash (keys entity))} [new-field entity param-ctx])))]

    [:div {:class (str "forms-list " (name (:layout param-ctx)))}
     (widget/render-anchors (->> anchors
                                 (remove :anchor/repeating?)
                                 (remove :anchor/attribute)
                                 (remove :anchor/render-inline?))
                            (dissoc param-ctx :isComponent))
     (concat fields [magic-new-field])
     (widget/render-inline-anchors (->> anchors
                                        (remove :anchor/repeating?)
                                        (remove :anchor/attribute)
                                        (filter :anchor/render-inline?))
                                   (dissoc param-ctx :isComponent))]))
