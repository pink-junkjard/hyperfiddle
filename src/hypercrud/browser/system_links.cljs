(ns hypercrud.browser.system-links
  (:require [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.types :refer [->DbId ->DbVal ->EntityRequest]]
            [hypercrud.ui.form-util :as form-util]))


(defn system-link? [link-dbid]
  (map? (:id link-dbid)))


(defn system-link-id->parent-link-dbid [system-link-dbid]
  (->DbId (get system-link-dbid :parent-link-id)
          (get system-link-dbid :parent-link-conn-id)))


(defn system-edit-link-dbid [entity-conn-id link-name parent-link-dbid]
  (->DbId {:parent-link-id (-> parent-link-dbid :id)
           :parent-link-conn-id (-> parent-link-dbid :conn-id)
           :link/ident :system-edit
           :link/name link-name
           :entity-conn-id entity-conn-id}
          (-> parent-link-dbid :conn-id)))


(defn system-edit-attr-link-dbid [find-element-id attr-id parent-link-dbid]
  (->DbId {:parent-link-id (-> parent-link-dbid :id)
           :parent-link-conn-id (-> parent-link-dbid :conn-id)
           :link/ident :system-edit-attr
           ;find-element-id
           ;field-id
           }
          (-> parent-link-dbid :conn-id)))


(defn system-edit-link [connection-dbid link-name parent-link]
  {:db/id (system-edit-link-dbid (:id connection-dbid) link-name (:db/id parent-link))
   :link/name link-name
   :hypercrud/owner (:hypercrud/owner parent-link)
   :link/request {:link-entity/connection {:db/id connection-dbid}}})


(defn system-edit-attr-link [find-element attr parent-link]
  {:db/id (system-edit-attr-link-dbid (-> find-element :db/id :id) (get-in attr [:db/id :id]) (:db/id parent-link))
   :link/name (str "edit " (:find-element/name find-element) " " (:attribute/ident attr))
   :hypercrud/owner (:hypercrud/owner parent-link)
   :link/request {:link-entity/connection (:find-element/connection find-element)}})


(defn system-anchors
  "provide the system links (edit, new, remove). sub-queries (e.g. combo boxes) will get the old pulled-tree.
  Since we only changed link, this is only interesting for the hc-in-hc case.

  todo don't overlay system links on system links"
  [parent-link result param-ctx]
  (let [colspec (form-util/determine-colspec result parent-link param-ctx) ; colspec can be empty if result is empty and no form.
        find-elements (form-util/find-elements-by-name (:link/request parent-link))]
    (case (links/link-type parent-link)
      :link-query
      (let [edit-links (->> find-elements
                            (mapv (fn [[fe-name fe]]
                                    (let [connection-dbid (-> fe :find-element/connection :db/id)
                                          link-name (str "system-edit " fe-name)]
                                      {:anchor/prompt (str fe-name)
                                       :anchor/ident (keyword fe-name)
                                       :anchor/link (system-edit-link connection-dbid link-name parent-link)
                                       :anchor/repeating? true
                                       :anchor/find-element fe
                                       :anchor/formula (pr-str {:entity-dbid-s (pr-str `(fn [~'ctx]
                                                                                          (get-in ~'ctx [:result ~fe-name :db/id])))})}))))

            edit-attr-links (->> (partition 3 colspec)      ; driven by colspec, not find elements, because what matters is what's there.
                                 (mapcat (fn [[fe-name ident maybe-field]]
                                           (let [fe (get find-elements fe-name)
                                                 attr ((:schema param-ctx) ident)]
                                             (case (-> attr :attribute/valueType :db/ident)
                                               :db.type/ref [{:anchor/prompt (str "edit " ident)
                                                              :anchor/ident (keyword (str fe-name (form-util/css-slugify (str ident))))
                                                              :anchor/link (system-edit-attr-link fe attr parent-link)
                                                              :anchor/repeating? true
                                                              :anchor/find-element fe
                                                              :anchor/attribute attr
                                                              :anchor/formula (pr-str {:entity-dbid-s (pr-str '(fn [ctx] nil))})}]
                                               nil))))
                                 doall)
            create-links (->> find-elements
                              (mapv (fn [[fe-name fe]]
                                      (let [connection (:find-element/connection fe)
                                            connection-dbid (:db/id connection)
                                            link-name (str "system-create " (:database/ident connection))]
                                        {:anchor/prompt (str "create in " (:database/ident connection))
                                         :anchor/link (system-edit-link connection-dbid link-name parent-link)
                                         :anchor/repeating? false
                                         :anchor/formula (pr-str {:entity-dbid-s (pr-str `(fn [~'ctx]
                                                                                            (hc/*temp-id!* ~(:id connection-dbid))))})}))))]
        (concat create-links edit-links edit-attr-links))

      :link-entity []                              ; No system links yet for entity links. What will there be?
      [])))


(defn request-for-system-link [system-link-id]
  (let [parent-link-dbid (system-link-id->parent-link-dbid system-link-id)]
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
  (let [{:keys [entity-conn-id link-name]} system-link-id
        entity-conn-dbid (->DbId entity-conn-id hc/*root-conn-id*)
        ; system links are only generated on QueryRequests, so we don't need to determine the type of the parent link
        parent-link system-link-deps]
    (case (:link/ident system-link-id)
      :system-edit (system-edit-link entity-conn-dbid link-name parent-link))))
