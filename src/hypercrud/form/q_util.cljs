(ns hypercrud.form.q-util
  (:require [cljs.reader :as reader]
            [clojure.string :as string]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.types :refer [->DbVal ->EntityRequest ->QueryRequest]]
            [hypercrud.util :as util]
            [hypercrud.ui.form-util :as form-util]))


(defn safe-read-string [code-str]
  (try
    (reader/read-string code-str)                           ; this doesn't handle sharp-lambdas
    (catch :default e
      ;; Nothing to be done at this point -
      ;; this error must be caught by the widget before it is staged.
      (.warn js/console "bad formula " code-str e)
      nil)))


(defn parse-holes [q]
  (->> (util/parse-query-element q :in)
       ;; the string conversion should happen at the other side imo
       (map str)))

(defn parse-param-holes [q]
  (->> (parse-holes q)
       (remove #(string/starts-with? % "$"))))


(defn build-dbhole-lookup [link-query]
  (->> (:link-query/dbhole link-query)
       (map (fn [{:keys [:dbhole/name :dbhole/value]}]
              (if-not (or (empty? name) (nil? value))
                ; transform project-id into conn-id
                [name (->DbVal (-> value :db/id :id) nil)])))
       (into {})))


; type fill-hole = fn [hole-name param-ctx] => param
(defn build-params [fill-hole link-query param-ctx]
  (->> (some-> link-query :link-query/value safe-read-string)
       (parse-holes)                                        ; nil means '() and does the right thing
       (mapv (juxt identity #(fill-hole % param-ctx)))
       (into {})))


(defn read-eval-formulas [formulas-str]
  (if-not (empty? formulas-str)
    (let [formulas-map (safe-read-string formulas-str)]
      (util/map-values eval formulas-map))))                ; not eval-str, because the formula-map was already read


(defn run-formula [{formula :value error :error} param-ctx]
  (if error
    (throw error)                                           ; first error, lose the rest of the errors
    (if formula (formula param-ctx))                        ; can also throw, lose the rest
    ))


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
    ['* {:hypercrud/owner ['*]}]))


;todo rename and move? ->queryRequest
(defn query-value [q link-query params-map param-ctx]
  (let [params (build-params #(get params-map %) link-query param-ctx)
        find-elements (:link-query/find-element link-query)
        find-elements (-> find-elements (form-util/strip-forms-in-raw-mode param-ctx))
        pull-exp (->> find-elements
                      (mapv (juxt :find-element/name (fn [{:keys [:find-element/connection :find-element/form]}]
                                                       [(->DbVal (-> connection :db/id :id) nil) (form-pull-exp form)])))
                      (into {}))]
    (->QueryRequest q params pull-exp)))


(defn ->entityRequest [link-entity params-map]
  (let [dbid-s (:entity-dbid-s params-map)
        dbval (->DbVal (-> link-entity :link-entity/connection :db/id :id) nil)
        pull-exp (form-pull-exp (:link-entity/form link-entity))]
    (->EntityRequest dbid-s dbval pull-exp)))
