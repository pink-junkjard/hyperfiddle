(ns hypercrud.form.q-util
  (:require [cljs.reader :as reader]
            [clojure.string :as string]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hypercrud.util.core :as util]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.client.core :as hc]))


(defn safe-read-string [code-str]
  (try
    (reader/read-string code-str)                           ; this doesn't handle sharp-lambdas
    (catch :default e
      ; Nothing to be done at this point -
      ; this error must be caught by the widget before it is staged.
      ;(.warn js/console "bad formula " code-str e)
      ; Happens as you type sometimes e.g. validated edn input.
      nil)))


(defn parse-holes [q]
  {:pre [(vector? q)]
   :post [(vector? %)]}
  (->> (util/parse-query-element q :in)
       ;; the string conversion should happen at the other side imo
       (mapv str)))

(defn parse-param-holes [q]
  (->> (parse-holes q)
       (remove #(string/starts-with? % "$"))))


(defn build-dbhole-lookup [link-query param-ctx]
  (->> (:link-query/dbhole link-query)
       (map (fn [{:keys [:dbhole/name :dbhole/value]}]
              (if-not (or (empty? name) (nil? value))
                ; transform project-id into conn-id
                [name (hc/db (:response param-ctx) (-> value :db/id :id) (get-in param-ctx [:branches (-> value :db/id :id)]))])))
       (into {})))

(defn safe-parse-query-validated [link-query]
  ; return monad and display the error to the widget?
  ; Should try not to even stage bad queries. If it happens though,
  ; we can draw the server error. Why can't we even get to server error now?
  (let [q (some-> link-query :link-query/value safe-read-string)]
    (if (vector? q)
      q
      [])))

; type fill-hole = fn [hole-name param-ctx] => param
(defn build-params [fill-hole link-query param-ctx]
  (try
    (->> (safe-parse-query-validated link-query)
         (parse-holes)                                      ; nil means '() and does the right thing
         (mapv (juxt identity #(fill-hole % param-ctx)))
         (into {}))
    (catch :default e
      {})))                                                 ; e.g. `:find is not ISeqable`

(defn run-formula! [{formula! :value error :error} param-ctx]
  (if error
    (throw error)                                           ; first error, lose the rest of the errors
    (if formula! (formula! param-ctx))))                    ; can also throw, lose the rest


(defn form-pull-exp [form]
  (if form
    (concat
      [:db/id {:hypercrud/owner ['*]}]
      (->> (:form/field form)
           (mapv #(-> % :field/attribute :attribute/ident))
           (set)
           (remove #{:hypercrud/owner})                     ; in meta-fiddle this is part of the form, but we want to hydrate deeper always.
           (remove nil?)))

    ; should we hydrate one level deeper for refs in undressed mode? nah
    ; we don't have the info to know which ref attrs might be used; its not really possible to do this.
    ; If you want pretty select options, you should model the options query and form.
    ; Now we are using sys-edit-attr render-inline to hydrate this level accurately with a form.
    ['* {:hypercrud/owner ['*]}]))


(defn ->queryRequest [q link-query params-map param-ctx]
  (let [params (build-params #(get params-map %) link-query param-ctx)
        find-elements (:link-query/find-element link-query)
        find-elements (-> find-elements (form-util/strip-forms-in-raw-mode param-ctx))
        pull-exp (->> find-elements
                      (mapv (juxt :find-element/name
                                  (fn [fe]
                                    (let [conn-id (-> fe :find-element/connection :db/id :id)]
                                      [(hc/db (:response param-ctx) conn-id (get-in param-ctx [:branches conn-id]))
                                       (form-pull-exp (:find-element/form fe))]))))
                      (into {}))]
    (->QueryRequest q params pull-exp)))


(defn ->entityRequest [link-entity query-params param-ctx]
  #_(assert (:entity query-params))                         ;-- Commented because we are requesting invisible things that the UI never tries to render - can be fixed
  #_(assert (:conn-id (:entity query-params)))
  (assert (-> link-entity :link-entity/connection :db/id :id))
  (->EntityRequest
    (:entity query-params)
    (:a query-params)
    (let [conn-id (-> link-entity :link-entity/connection :db/id :id)]
      (hc/db (:response param-ctx) conn-id (get-in param-ctx [:branches conn-id])))
    (form-pull-exp (:link-entity/form link-entity))))
