(ns hypercrud.browser.system-links
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx]
            [hypercrud.types :refer [->DbId]]))


(defn system-edit-link-dbid [find-element-id parent-link-dbid]
  (->DbId [(-> parent-link-dbid :id)
           (-> parent-link-dbid :conn-id)
           :system-edit
           find-element-id]
          (-> parent-link-dbid :conn-id)))


(defn system-create-link-dbid [find-element-id parent-link-dbid]
  (->DbId [(-> parent-link-dbid :id)
           (-> parent-link-dbid :conn-id)
           :system-create
           find-element-id]
          (-> parent-link-dbid :conn-id)))


(def system-link-owner nil)


(defn system-edit-link [find-element parent-link-dbid]
  ; todo we can remove temp-id!
  (let [new-find-element-dbid (hc/*temp-id!* hc/*root-conn-id*)
        new-dbhole-dbid (hc/*temp-id!* hc/*root-conn-id*)]
    {:db/id (system-edit-link-dbid (-> find-element :db/id :id) parent-link-dbid)
     :link/prompt (str "edit " (:find-element/name find-element))
     :hypercrud/owner system-link-owner
     :link/query (pr-str '[:find ?e :in $ ?e :where [?e]])
     :link/single-result-as-entity? true
     :link/dbhole #{{:db/id new-dbhole-dbid
                     :dbhole/name "$"
                     :dbhole/value (:find-element/connection find-element)}}
     :link/find-element #{{:db/id new-find-element-dbid
                           :find-element/name "?e"
                           :find-element/connection (:find-element/connection find-element)
                           :find-element/form (:find-element/form find-element)}}}))


(defn system-create-link [find-element parent-link-dbid]
  {:db/id (system-create-link-dbid (-> find-element :db/id :id) parent-link-dbid)
   :link/prompt (str "create " (:find-element/name find-element))
   :hypercrud/owner system-link-owner
   :link/tx-fn (let [hc-a (let [fields (-> find-element :find-element/form :form/field)
                                ; attempt to use a string or keyword field
                                ; todo unnecessary once valid nil cases are created/tested for all types
                                field (or (->> fields
                                               (filter (fn [field]
                                                         (contains? #{:db.type/string :db.type/keyword}
                                                                    (-> field :field/attribute :attribute/valueType :db/ident))))
                                               first)
                                          (first fields))]
                            (:field/attribute field))
                     a (-> hc-a :attribute/ident)
                     valueType (-> hc-a :attribute/valueType :db/ident)
                     v (condp = valueType
                         :db.type/keyword :nil
                         :db.type/string ""
                         :db.type/boolean false
                         :db.type/long 0
                         :db.type/bigint 0
                         :db.type/float 0.0
                         :db.type/double 0.0
                         :db.type/bigdec 0
                         :db.type/ref '(hypercrud.types/->DbId nil nil) ;todo
                         :db.type/instant '(js/Date nil)
                         :db.type/uuid '(random-uuid)
                         :db.type/uri ""
                         :db.type/bytes [])
                     conn-id (-> find-element :find-element/connection :db/id :id)]
                 (pr-str `(fn [~'ctx]
                            (let [~'e (hypercrud.client.core/*temp-id!* ~conn-id)]
                              {:tx [[:db/add ~'e ~a ~v]]}))))})


(defn overlay-system-links-tx
  "remove the user links and provide the system links (edit, new, remove)"
  [parent-link]
  (let [link-ctxs (->> (:link/find-element parent-link)
                       (mapcat (fn [find-element]
                                 (let [edit-link-ctx-dbid (hc/*temp-id!* (-> parent-link :db/id :conn-id))
                                       create-link-ctx-dbid (hc/*temp-id!* (-> parent-link :db/id :conn-id))]
                                   [{:db/id edit-link-ctx-dbid
                                     :link-ctx/ident :edit
                                     :link-ctx/link (system-edit-link find-element (:db/id parent-link))
                                     :link-ctx/repeating? true
                                     :link-ctx/formula (pr-str {"?e" (pr-str `(fn [~'ctx]
                                                                                (get-in ~'ctx [:result ~(:find-element/name find-element) :db/id])))})}
                                    {:db/id create-link-ctx-dbid
                                     :link-ctx/ident :create
                                     :link-ctx/link (system-create-link find-element (:db/id parent-link))
                                     :link-ctx/repeating? false}])))
                       (into #{}))]
    (assoc parent-link :link/link-ctx link-ctxs)))
