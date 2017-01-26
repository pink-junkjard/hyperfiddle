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


(defn manufacture-system-edit-tx [find-element parent-link-dbid]
  (let [new-find-element-dbid (hc/*temp-id!* hc/*root-conn-id*)
        new-dbhole-dbid (hc/*temp-id!* hc/*root-conn-id*)]
    (concat
      (tx/entity->statements
        {:db/id (system-edit-link-dbid (-> find-element :db/id :id) parent-link-dbid)
         :link/prompt (str "edit " (:find-element/name find-element))
         :hypercrud/owner system-link-owner
         :link/query (pr-str '[:find ?e :in $ ?e :where [?e]])
         :link/single-result-as-entity? true
         :link/dbhole #{new-dbhole-dbid}
         :link/find-element #{new-find-element-dbid}})
      (tx/entity->statements {:db/id new-find-element-dbid
                              :find-element/name "?e"
                              :find-element/connection (:find-element/connection find-element)
                              :find-element/form (:find-element/form find-element)})
      (tx/entity->statements {:db/id new-dbhole-dbid
                              :dbhole/name "$"
                              :dbhole/value (:find-element/connection find-element)}))))


(defn manufacture-system-create-tx [find-element parent-link-dbid]
  (let [new-find-element-dbid (hc/*temp-id!* hc/*root-conn-id*)
        new-dbhole-dbid (hc/*temp-id!* hc/*root-conn-id*)]
    (concat
      (tx/entity->statements
        {:db/id (system-create-link-dbid (-> find-element :db/id :id) parent-link-dbid)
         :link/prompt (str "create " (:find-element/name find-element))
         :hypercrud/owner system-link-owner
         :link/query (pr-str '[:find ?e :in $ ?e :where [?e]])
         :link/single-result-as-entity? true
         :link/dbhole #{new-dbhole-dbid}
         :link/find-element #{new-find-element-dbid}})
      (tx/entity->statements {:db/id new-find-element-dbid
                              :find-element/name "?e"
                              :find-element/connection (:find-element/connection find-element)
                              :find-element/form (:find-element/form find-element)})
      (tx/entity->statements {:db/id new-dbhole-dbid
                              :dbhole/name "$"
                              :dbhole/value (:find-element/connection find-element)}))))


(defn overlay-system-links-tx
  "remove the user links and provide the system links (edit, new, remove)"
  [parent-link]
  (let [link-stuff (mapv (fn [find-element]
                           (let [edit-link-dbid (system-edit-link-dbid (-> find-element :db/id :id) (:db/id parent-link))
                                 edit-link-ctx-dbid (hc/*temp-id!* (-> parent-link :db/id :conn-id))
                                 create-link-dbid (system-create-link-dbid (-> find-element :db/id :id) (:db/id parent-link))
                                 create-link-ctx-dbid (hc/*temp-id!* (-> parent-link :db/id :conn-id))
                                 tx (concat (manufacture-system-edit-tx find-element (:db/id parent-link))
                                            ; need to recurse components here
                                            (tx/entity->statements
                                              {:db/id edit-link-ctx-dbid
                                               :link-ctx/ident :edit
                                               :link-ctx/link edit-link-dbid
                                               :link-ctx/repeating? true
                                               :link-ctx/formula (pr-str {"?e" (pr-str `(fn [~'ctx]
                                                                                          (get-in ~'ctx [:result ~(:find-element/name find-element) :db/id])))})})
                                            (manufacture-system-create-tx find-element (:db/id parent-link))
                                            (tx/entity->statements
                                              {:db/id create-link-ctx-dbid
                                               :link-ctx/ident :create
                                               :link-ctx/link create-link-dbid
                                               :link-ctx/repeating? false
                                               :link-ctx/formula (pr-str {"?e" (pr-str '(constantly nil))})}))]
                             [[edit-link-ctx-dbid create-link-ctx-dbid] tx]))
                         (:link/find-element parent-link))]
    (concat
      (mapcat second link-stuff)
      ; remove user links and add system links
      (tx/retract (:db/id parent-link) :link/link-ctx (mapv :db/id (-> parent-link :link/link-ctx)))
      (tx/add (:db/id parent-link) :link/link-ctx (mapcat first link-stuff)))))
