(ns hypercrud.ui.options
  (:require [cats.core :as cats :refer-macros [mlet]]
            [cats.monad.either :as either :refer-macros [try-either]]
            [datascript.parser :as parser]))


(defn default-label-renderer [v ctx]
  (cond
    (instance? cljs.core/Keyword v) (name v)
    :else (str v))
  #_(condp (fn [x c] (instance? c x)) v
      cljs.core/Keyword (name v)
      (str v)))

(defn build-label [ordered-fes relation ctx]
  (->> (map (fn [cell-data fe]
              (->> (:fields fe)
                   (mapv (fn [field {:keys [attribute]}]
                           (let [value ((:cell-data->value field) cell-data)
                                 renderer (get-in ctx [:fields attribute :label-renderer] default-label-renderer)]
                             (try-either (renderer value ctx)))))))
            relation
            ordered-fes)
       (apply concat)
       (cats/sequence)
       (cats/fmap (fn [labels]
                    (->> labels
                         (interpose ", ")
                         (remove nil?)
                         (apply str))))))

(defn options-from-relations [relations ordered-fes ctx]
  (conj
    (->> relations
         (mapv (fn [relation]
                 (let [label (-> (build-label ordered-fes relation ctx)
                                 (either/branch (fn [e] (pr-str e)) identity))]
                   [(:db/id (first relation)) label])))
         (sort-by second)
         (map (fn [[id label]]
                [:option {:key (str id) :value id} label])))
    [:option {:key :blank :value ""} "--"]))

; takes a dom event and returns a value (id)
(defn options-e->id [e]
  (let [select-value (.-target.value e)]
    (when (not= "" select-value)
      (let [id (js/parseInt select-value 10)]
        (if (< id 0) (str id) id)))))

(defn options [result ordered-fes ctx]
  (case (get-in ctx [:fiddle :request/type])
    :entity (either/left "Only fiddle type `query` is supported for options")
    :blank (either/left "Only fiddle type `query` is supported for options")
    :query (mlet [{:keys [qfind]} (try-either (parser/parse-query (get-in ctx [:request :query])))]
             (condp = (type qfind)
               datascript.parser.FindRel (either/right (options-from-relations result ordered-fes ctx))
               datascript.parser.FindColl (either/right (options-from-relations (map vector result) ordered-fes ctx))

               datascript.parser.FindTuple
               (either/left "Tuples are unsupported for options. Please fix your options query to return a relation or collection")

               datascript.parser.FindScalar
               (either/left "Scalars are unsupported for options. Please fix your options query to return a relation or collection")))
    ; default
    (either/left "Only fiddle type `query` is supported for options")))
