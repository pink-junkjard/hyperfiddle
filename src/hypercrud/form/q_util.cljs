(ns hypercrud.form.q-util
  (:require [cljs.reader :as reader]
            [clojure.string :as string]
            [hypercrud.client.core :as hc]
            [hypercrud.util.core :as util]))


(defn safe-read-string [code-str]
  (try
    (if code-str (reader/read-string code-str))                    ; this doesn't handle sharp-lambdas
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


(defn build-dbhole-lookup [link param-ctx]
  (->> (:link-query/dbhole link)
       (map (fn [{:keys [:dbhole/name :dbhole/value]}]
              (if-not (or (empty? name) (nil? value))
                ; transform project-id into conn-id
                [name (hc/db (:peer param-ctx) (-> value :db/id :id) (get-in param-ctx [:branches (-> value :db/id :id)]))])))
       (into {})))

(defn safe-parse-query-validated [link]
  ; return monad and display the error to the widget?
  ; Should try not to even stage bad queries. If it happens though,
  ; we can draw the server error. Why can't we even get to server error now?
  (let [q (some-> link :link-query/value safe-read-string)]
    (if (vector? q)
      q
      [])))

; type fill-hole = fn [hole-name param-ctx] => param
(defn build-params [fill-hole link param-ctx]
  (try
    (->> (safe-parse-query-validated link)
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
