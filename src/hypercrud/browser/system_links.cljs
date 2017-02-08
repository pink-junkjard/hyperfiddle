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
    (let [anchors (->> (get-in parent-link [:link/request :link-query/find-element])
                       (mapcat (fn [find-element]
                                 (let [edit-anchor-dbid (hc/*temp-id!* (-> parent-link :db/id :conn-id))
                                       create-anchor-dbid (hc/*temp-id!* (-> parent-link :db/id :conn-id))]
                                   [{:db/id edit-anchor-dbid
                                     :anchor/ident :edit
                                     :anchor/prompt (str "edit " (:find-element/name find-element))
                                     :anchor/link (system-edit-link find-element (:db/id parent-link))
                                     :anchor/repeating? true
                                     :anchor/formula (pr-str {:entity-dbid-s (pr-str `(fn [~'ctx]
                                                                                        (get-in ~'ctx [:result ~(:find-element/name find-element) :db/id])))})}

                                    {:db/id create-anchor-dbid
                                     :anchor/ident :create
                                     :anchor/prompt (str "create " (:find-element/name find-element))
                                     :anchor/link (system-edit-link find-element (:db/id parent-link))
                                     :anchor/repeating? false
                                     :anchor/formula (pr-str {:entity-dbid-s (pr-str `(fn [~'ctx]
                                                                                        (hc/*temp-id!* ~(get-in find-element [:find-element/connection :db/id :id]))))})}])))
                       (into #{}))]
      (assoc parent-link :link/anchor anchors))

    :link-entity
    (dissoc parent-link :link/anchor)

    ; else
    (dissoc parent-link :link/anchor)))


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
                        {:hypercrud/owner ['*]
                         :link/request
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
