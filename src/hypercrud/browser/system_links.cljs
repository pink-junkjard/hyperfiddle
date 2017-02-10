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


(defn system-edit-field-link-dbid [find-element-id field-id parent-link-dbid]
  (->DbId [(-> parent-link-dbid :id)
           (-> parent-link-dbid :conn-id)
           :system-edit-field
           find-element-id
           field-id]
          (-> parent-link-dbid :conn-id)))


(defn system-edit-link [find-element parent-link]
  {:db/id (system-edit-link-dbid (-> find-element :db/id :id) (:db/id parent-link))
   :link/name (str "edit " (:find-element/name find-element))
   :hypercrud/owner (:hypercrud/owner parent-link)
   :link/request {:link-entity/connection (:find-element/connection find-element)}})


(defn system-edit-field-link [find-element field parent-link]
  {:db/id (system-edit-field-link-dbid (-> find-element :db/id :id) (get-in field [:db/id :id]) (:db/id parent-link))
   :link/name (str "edit " (:find-element/name find-element) " " (get-in field [:field/attribute :attribute/ident]))
   :hypercrud/owner (:hypercrud/owner parent-link)
   :link/request {:link-entity/connection (:find-element/connection find-element)
                  :link-entity/form (:find-element/form find-element)}})


(defn overlay-system-links-tx
  "remove the user links and provide the system links (edit, new, remove)"
  [parent-link]
  (condp = (links/link-type parent-link)
    :link-query
    (let [anchors (->> (get-in parent-link [:link/request :link-query/find-element])
                       (mapcat (fn [find-element]
                                 (let [edit-anchor-dbid (hc/*temp-id!* (-> parent-link :db/id :conn-id))
                                       create-anchor-dbid (hc/*temp-id!* (-> parent-link :db/id :conn-id))]
                                   (concat
                                     [{:db/id edit-anchor-dbid
                                       :anchor/prompt (str "edit " (:find-element/name find-element))
                                       :anchor/link (system-edit-link find-element parent-link)
                                       :anchor/repeating? true
                                       :anchor/find-element find-element
                                       :anchor/formula (pr-str {:entity-dbid-s (pr-str `(fn [~'ctx]
                                                                                          (get-in ~'ctx [:result ~(:find-element/name find-element) :db/id])))})}

                                      {:db/id create-anchor-dbid
                                       :anchor/prompt (str "create " (:find-element/name find-element))
                                       :anchor/link (system-edit-link find-element parent-link)
                                       :anchor/repeating? false
                                       :anchor/find-element find-element
                                       :anchor/formula (pr-str {:entity-dbid-s (pr-str `(fn [~'ctx]
                                                                                          (hc/*temp-id!* ~(get-in find-element [:find-element/connection :db/id :id]))))})}]
                                 #_    (->> (-> find-element :find-element/form :form/field)
                                          (filter #(= :db.type/ref (-> % :field/attribute :attribute/valueType)))
                                          (mapcat (fn [field]
                                                    [{:db/id "i dont think we need this"
                                                      :anchor/prompt "edit"
                                                      :anchor/link (system-edit-field-link find-element field parent-link)
                                                      :anchor/repeating? true
                                                      :anchor/find-element find-element
                                                      :anchor/field field
                                                      :anchor/formula (pr-str {:entity-dbid-s (pr-str '(fn [ctx] nil))})}])))))))
                       (into #{}))]
      (assoc parent-link :link/anchor anchors))

    :link-entity
    (dissoc parent-link :link/anchor)

    ; else
    (dissoc parent-link :link/anchor)))


(defn request-for-system-link [system-link-id]
  (let [[parent-link-id parent-link-conn-id] system-link-id
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
                        {:hypercrud/owner ['*]
                         :link/request
                         [:db/id
                          {:link-query/find-element [:db/id
                                                     :find-element/name
                                                     :find-element/connection
                                                     {:find-element/form form-pull-exp}]}]}]))))


(defn generate-system-link [system-link-id system-link-deps]
  (let [[_ _ system-link-name find-element-id] system-link-id
        parent-link system-link-deps
        ; system links are only generated on QueryRequests, so we don't need to determine the type of the parent link
        find-element (->> (get-in parent-link [:link/request :link-query/find-element])
                          (filter #(= find-element-id (-> % :db/id :id)))
                          first)]
    (condp = system-link-name
      :system-edit (system-edit-link find-element parent-link))))
