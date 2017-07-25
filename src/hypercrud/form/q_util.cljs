(ns hypercrud.form.q-util
  (:require [cljs.reader :as reader]
            [hypercrud.client.core :as hc]
            [hypercrud.util.core :as util]))


; deprecated
; use hypercrud.util.string/safe-read-string
(defn safe-read-string [code-str]
  (try
    (if code-str (reader/read-string code-str))             ; this doesn't handle sharp-lambdas
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

(defn build-dbhole-lookup [link param-ctx]
  (->> (:link-query/dbhole link)
       (map (fn [{:keys [:dbhole/name :dbhole/value]}]
              (if-not (or (empty? name) (nil? value))
                ; transform project-id into conn-id
                [name (hc/db (:peer param-ctx) (-> value :db/id :id) (get-in param-ctx [:branches (-> value :db/id :id)]))])))
       (into {})))

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
