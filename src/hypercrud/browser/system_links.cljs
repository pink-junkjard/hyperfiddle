(ns hypercrud.browser.system-links
  (:require [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.types :refer [->DbId ->DbVal ->EntityRequest]]))


(defn system-edit-link-dbid [find-element-id parent-link-dbid]
  (->DbId [(-> parent-link-dbid :id)
           (-> parent-link-dbid :conn-id)
           :system-edit
           find-element-id]
          (-> parent-link-dbid :conn-id)))


(def system-link-owner nil)


(defn system-edit-link [find-element parent-link-dbid]
  ; todo we can remove temp-id!
  (let [new-link-entity-dbid (hc/*temp-id!* hc/*root-conn-id*)]
    {:db/id (system-edit-link-dbid (-> find-element :db/id :id) parent-link-dbid)
     :link/name (str "edit " (:find-element/name find-element))
     :hypercrud/owner system-link-owner
     :link/request {:db/id new-link-entity-dbid
                    :link-entity/connection (:find-element/connection find-element)
                    :link-entity/form (:find-element/form find-element)}}))


(defn overlay-system-links-tx
  "remove the user links and provide the system links (edit, new, remove)"
  [parent-link]
  (condp = (links/link-type parent-link)
    :link-query
    (let [link-ctxs (->> (get-in parent-link [:link/request :link-query/find-element])
                         (mapcat (fn [find-element]
                                   (let [edit-link-ctx-dbid (hc/*temp-id!* (-> parent-link :db/id :conn-id))
                                         create-link-ctx-dbid (hc/*temp-id!* (-> parent-link :db/id :conn-id))]
                                     [{:db/id edit-link-ctx-dbid
                                       :link-ctx/ident :edit
                                       :link-ctx/prompt (str "edit " (:find-element/name find-element))
                                       :link-ctx/link (system-edit-link find-element (:db/id parent-link))
                                       :link-ctx/repeating? true
                                       :link-ctx/formula (pr-str {:entity-dbid (pr-str `(fn [~'ctx]
                                                                                          (get-in ~'ctx [:result ~(:find-element/name find-element) :db/id])))})}
                                      {:db/id create-link-ctx-dbid
                                       :link-ctx/ident :create
                                       :link-ctx/prompt (str "create " (:find-element/name find-element))
                                       :link-ctx/tx-fn (let [hc-a (let [fields (-> find-element :find-element/form :form/field)
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
                                                                      {:tx [[:db/add ~'e ~a ~v]]}))))
                                       :link-ctx/repeating? false}])))
                         (into #{}))]
      (assoc parent-link :link/link-ctx link-ctxs))

    :link-entity
    (dissoc parent-link :link/link-ctx)))


(defn request-for-system-link [system-link-dbid]
  (let [[parent-link-id parent-link-conn-id] system-link-dbid
        parent-link-dbid (->DbId parent-link-id parent-link-conn-id)]
    (->EntityRequest parent-link-dbid
                     (->DbVal hc/*root-conn-id* nil)
                     (let [form-pull-exp ['* {:form/field
                                              ['*
                                               {:field/attribute ['*
                                                                  {:attribute/valueType [:db/id :db/ident]}
                                                                  {:attribute/cardinality [:db/id :db/ident]}
                                                                  {:attribute/unique [:db/id :db/ident]}]}]}]]
                       [:db/id
                        {:link/request
                         [:db/id
                          {:link-query/find-element [:db/id
                                                     :find-element/name
                                                     :find-element/connection
                                                     {:find-element/form form-pull-exp}]}]}]))))


(defn generate-system-link [system-link-dbid system-link-deps]
  (let [[_ _ system-link-name find-element-id] system-link-dbid
        parent-link system-link-deps
        ; system links are only generated on QueryRequests, so we don't need to determine the type of the parent link
        find-element (->> (get-in parent-link [:link/request :link-query/find-element])
                          (filter #(= find-element-id (-> % :db/id :id)))
                          first)]
    (condp = system-link-name
      :system-edit (system-edit-link find-element (:db/id parent-link)))))
