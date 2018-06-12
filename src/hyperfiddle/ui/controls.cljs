(ns hyperfiddle.ui.controls
  (:refer-clojure :exclude [boolean keyword long])
  (:require
    [clojure.set :as set]
    [contrib.datomic-tx :as tx]
    [contrib.reactive :as r]
    [contrib.string :refer [empty->nil]]
    [contrib.ui :refer [code-block code-inline-block edn-block edn-inline-block]]
    [contrib.ui.input :as input]
    [contrib.ui.recom-date :refer [recom-date]]))


(defn keyword [value ctx props]
  [:div
   (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
         on-change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))]
     [input/keyword-input* value on-change! props])])

(defn string [value ctx props]
  [:div
   (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
         on-change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) (empty->nil %)))]
     [input/input* value on-change! props])])

(defn long [value ctx props]
  (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
        on-change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))]
    [:div
     [input/validated-input
      value on-change!
      #(js/parseInt % 10) (fnil str "")
      #(or #_(= "" %) (integer? (js/parseInt % 10)))
      props]]))

(defn ^:export boolean [value ctx props]
  [:div
   (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
         change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx)
                                                            @(:hypercrud.browser/fat-attribute ctx)
                                                            (not value)))]
     (contrib.ui/checkbox value change! props))])

(defn select-boolean* [value props ctx]
  (let [option-props {:disabled (or (cljs.core/boolean (:read-only props))
                                    (nil? @(r/cursor (:cell-data ctx) [:db/id])))}
        props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
               :value (if (nil? value) "" (str value))
               ;; reconstruct the typed value
               :on-change #(let [v (case (.-target.value %)
                                     "" nil
                                     "true" true
                                     "false" false)]
                             ((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) v)))}]
    [:select (dissoc props :label-fn)
     [:option (assoc option-props :key true :value "true") "True"]
     [:option (assoc option-props :key false :value "false") "False"]
     [:option (assoc option-props :key :nil :value "") "--"]]))

(defn ^:export tristate-boolean [value ctx props]
  [:div
   (select-boolean* value props ctx)])

(defn id* [value ctx props]
  (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
        on-change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))]
    (input/id-input value on-change! props)))

(defn dbid [value ctx props]
  [:div (id* value ctx props)])

(defn text [value ctx props]
  [:div
   [:span
    (case @(r/cursor (:hypercrud.browser/fat-attribute ctx) [:db/cardinality :db/ident])
      :db.cardinality/many (map pr-str value)
      (pr-str value))]])

;/*.value span.text {*/
;    /*margin: 3px 0;*/
;/*min-height: 15px;*/
;/*display: inline-block;*/
;/*line-height: normal;*/
;/*vertical-align: top;*/
;/*padding-left: 5px;*/
;/*}*/

(defn ^:export instant [value ctx props]
  [:div
   (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
         change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))
         widget (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                  :hyperfiddle.ui.layout/block recom-date
                  :hyperfiddle.ui.layout/table recom-date)]
     [widget value change! props])])

(defn ^:export code [value ctx props]
  (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
        change! (fn [%]
                  (let [tx (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) (empty->nil %))]
                    ((:user-with! ctx) tx)))]
    [:div
     (let [control (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                     :hyperfiddle.ui.layout/block code-block
                     :hyperfiddle.ui.layout/table code-inline-block)]
       ; backwards args - props last
       [control props value change!])]))

(defn ^:export markdown-editor [value ctx props]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]
        props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
        change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) (empty->nil %)))]
    [:div
     (let [widget (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                    :hyperfiddle.ui.layout/block code-block
                    :hyperfiddle.ui.layout/table code-inline-block)
           props (assoc props :mode "markdown" :lineWrapping true)]
       ; backwards args - props last
       [widget props value change!])]))

(defn ^:export edn-many [value ctx props]
  (let [valueType @(r/cursor (:hypercrud.browser/fat-attribute ctx) [:db/valueType :db/ident])
        value (-> (if (= valueType :db.type/ref)
                    (map :db/id value)
                    value)
                  set)
        props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
        change! (fn [user-val]
                  (let [user-val (set user-val)
                        rets (set/difference value user-val)
                        adds (set/difference user-val value)]
                    ((:user-with! ctx) (tx/edit-entity (-> ctx :cell-data deref :db/id)
                                                       (:hypercrud.browser/attribute ctx)
                                                       rets adds))))
        widget (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                 :hyperfiddle.ui.layout/block edn-block
                 :hyperfiddle.ui.layout/table edn-inline-block)]
    [:div
     [widget value change! props]]))

(defn ^:export edn [value ctx props]
  (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
        change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))
        widget (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                 :hyperfiddle.ui.layout/table edn-inline-block
                 :hyperfiddle.ui.layout/block edn-block)]
    [:div
     [widget value change! props]]))

(defn textarea* [{:keys [value on-change] :as props}]       ; unused
  (let [on-change #(let [newval (.. % -target -value)]
                     (on-change [value] [newval]))]
    [:textarea (assoc props :on-change on-change)]))
