(ns hypercrud.ui.form-util
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [clojure.string :as string]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.ui.markdown :refer [markdown]]
            [hypercrud.ui.tooltip :as tooltip]
            [hypercrud.util.core :as util]))

(defn css-slugify [s]
  ; http://stackoverflow.com/a/449000/959627
  (-> s
      (string/replace ":" "-")
      (string/replace "/" "-")
      (string/replace " " "-")))

(defn strip-form-in-raw-mode [fe param-ctx]
  (if (= @(:display-mode param-ctx) :root)
    (dissoc fe :find-element/form)
    fe))

(defn get-ordered-find-elements [link param-ctx]
  (mlet [fes (case (:request/type link)
               :query (let [find-element-lookup (->> (:link-query/find-element link)
                                                     (map (juxt :find-element/name identity))
                                                     (into {}))]
                        (mlet [q (q-util/safe-parse-query-validated link)]
                          (->> (util/parse-query-element q :find)
                               (mapv str)
                               (mapv #(get find-element-lookup % {:find-element/name %}))
                               (cats/return))))
               :entity (either/right [(or (->> (:link-query/find-element link)
                                               (filter #(= (:find-element/name %) "entity"))
                                               first)
                                          {:find-element/name "entity"})])
               (either/right []))]
    (->> fes
         (map (fn [fe] (update fe :find-element/connection #(or % "$"))))
         (map #(strip-form-in-raw-mode % param-ctx))
         (cats/return))))

(defn build-props [field anchors param-ctx]
  ; why does this need the field - it needs the ident for readonly in "Edit Anchors"
  ; todo clean this interface up
  {:read-only ((get param-ctx :read-only) (:attribute param-ctx) param-ctx)})

(defn attribute-human [attr]
  (-> attr
      (dissoc :db/id)
      (util/update-existing :db/cardinality :db/ident)
      (util/update-existing :db/valueType :db/ident)
      (util/update-existing :db/unique :db/ident)
      (util/update-existing :attribute/hc-type :hc-type/name)))

(defn field-label [field param-ctx]
  (let [docstring (:field/doc field)
        field-prompt (util/fallback empty? (:field/prompt field) (-> param-ctx :attribute :db/ident str))
        display-mode @(:display-mode param-ctx)]
    [tooltip/hover-popover-managed
     {:label (case display-mode
               ; (auto-control maybe-field anchors props param-ctx)
               :user (if-not (empty? docstring) (markdown docstring #()))
               :xray [:pre (util/pprint-str (attribute-human (:attribute param-ctx)) 50)])}
     [:span {:class (case display-mode
                      :user (if-not (empty? docstring) "help")
                      :xray "help")} field-prompt]]
    #_[:div
       (let [is-ref? (coll? value)]
         (if is-ref?
           [tooltip/click-popover-managed
            {:body [code-editor/code-editor* (util/pprint-str value 100) nil {:readOnly true}]}
            [:a {:href "javascript:void 0;"} "§"]]))
       " "
       ]))