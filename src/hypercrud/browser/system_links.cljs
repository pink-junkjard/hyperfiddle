(ns hypercrud.browser.system-links
  (:require [hypercrud.browser.link-util :as link-util]
            [hypercrud.client.core :as hc]
            [hypercrud.types :refer [->DbId ->DbVal ->EntityRequest]]
            [hypercrud.ui.form-util :as form-util]))


(defn system-link? [link-dbid]
  (map? (:id link-dbid)))

(defn link-query-system-edit [parent-link fe]
  (assert parent-link) (assert (:db/id parent-link))
  (assert fe) (assert (:db/id fe)) (assert (:find-element/connection fe))
  {:db/id (->DbId {:ident :system-edit
                   :parent-link (:db/id parent-link)
                   :find-element (:db/id fe)}
                  (-> parent-link :db/id :conn-id))
   :hypercrud/owner (:hypercrud/owner parent-link)
   :link/name (str "edit ")                                 ; hmm
   :link/request {:link-entity/connection (:find-element/connection fe)}})

(defn link-entity-system-edit [parent-link conn]
  (assert parent-link) (assert (:db/id parent-link))
  (assert conn) (assert (-> conn :db/id :id))
  {:db/id (->DbId {:ident :system-edit-attr
                   :parent-link (:db/id parent-link)
                   :conn (-> conn :db/id)}
                  (-> parent-link :db/id :conn-id))
   :hypercrud/owner (:hypercrud/owner parent-link)
   :link/name (str "edit ")                                 ; hmm
   :link/request {:link-entity/connection conn}})

(defn link-query-system-edit-attr [parent-link fe attr]
  (assert parent-link) (assert (:db/id parent-link))
  (assert fe) (assert (:db/id fe)) (assert (:find-element/connection fe))
  (assert attr) (assert (:db/id attr))
  {:db/id (->DbId {:ident :system-edit-attr
                   :parent-link (:db/id parent-link)
                   :find-element (:db/id fe)
                   :attribute (:db/id attr)}
                  (-> parent-link :db/id :conn-id))
   :link/name (str "edit " (:find-element/name fe) " " (:attribute/ident attr))
   :hypercrud/owner (:hypercrud/owner parent-link)
   :link/request {:link-entity/connection (:find-element/connection fe)}})

(defn link-entity-system-edit-attr [parent-link conn attr]
  (assert parent-link) (assert (:db/id parent-link))
  (assert conn) (assert (-> conn :db/id :id))
  ;(assert attr) (assert (:db/id attr)) ; schema not hydrated yet?
  {:db/id (->DbId {:ident :system-edit-attr
                   :parent-link (:db/id parent-link)
                   :conn (-> conn :db/id)
                   :attribute (:db/id attr)}
                  (-> parent-link :db/id :conn-id))
   :link/name (str "edit " (:attribute/ident attr))
   :hypercrud/owner (:hypercrud/owner parent-link)
   :link/request {:link-entity/connection conn}})


(defn system-anchors
  "All sys links are :anchor/ident :sys, so they can be matched and merged with user-anchors.
  Matching is determined by [repeat? entity attribute ident]"
  [parent-link result param-ctx]
  (let [colspec (form-util/determine-colspec result parent-link param-ctx) ; colspec can be empty if result is empty and no form.

        type (link-util/link-type parent-link)

        find-elements (case type
                        :link-query (->> (form-util/get-ordered-find-elements parent-link param-ctx)
                                         (mapv (juxt :find-element/name identity))
                                         (into {}))
                        :link-entity nil
                        :link-blank nil)

        ; entity links for link-query (have fe)
        ; or, entity links for link-entity
        entity-links (case type
                       :link-query
                       (->> find-elements
                            (mapcat (fn [[fe-name fe]]
                                      [{:anchor/prompt (str fe-name)
                                        :anchor/ident :sys
                                        :anchor/link (link-query-system-edit parent-link fe)
                                        :anchor/repeating? true
                                        :anchor/find-element fe}
                                       ; create links mirror edit links but repeating false, see auto-formula.
                                       ; This is because the connection comes from the find-element, and when merging
                                       ; sys links we match on the find-element.
                                       {:anchor/prompt (str "new " fe-name)
                                        :anchor/ident :sys
                                        :anchor/link (link-query-system-edit parent-link fe)
                                        :anchor/repeating? false
                                        :anchor/find-element fe
                                        :anchor/render-inline? false}
                                       {:anchor/prompt (str "remove " fe-name)
                                        :anchor/ident :remove
                                        :anchor/link nil
                                        :anchor/repeating? true
                                        :anchor/find-element fe
                                        :anchor/render-inline? false}]))
                            doall)

                       :link-entity
                       (let [conn (or
                                    (-> parent-link :link/request :link-entity/connection)
                                    (let [dbid-s (-> param-ctx :query-params :entity)]
                                      {:db/id (->DbId (if (vector? dbid-s)
                                                        (:conn-id (first dbid-s))
                                                        (:conn-id dbid-s))
                                                      nil #_"ignored in the place we need it, ->entityRequest")}))]
                         [{:anchor/prompt "edit"
                           :anchor/ident :sys
                           :anchor/link (link-entity-system-edit parent-link conn)
                           :anchor/repeating? true
                           :anchor/find-element nil}
                          {:anchor/prompt "new"
                           :anchor/ident :sys
                           :anchor/link (link-entity-system-edit parent-link conn)
                           :anchor/repeating? false
                           :anchor/find-element nil
                           :anchor/render-inline? false}
                          {:anchor/prompt "remove"
                           :anchor/ident :remove
                           :anchor/link nil
                           :anchor/repeating? true
                           :anchor/render-inline? false}])

                       :link-blank [])


        attr-links (case type
                     :link-query
                     (->> (partition 4 colspec)             ; driven by colspec, not find elements, because what matters is what's there.
                          (mapcat (fn [[conn fe-name ident maybe-field]]
                                    (let [fe (get find-elements fe-name) ; can be nil now
                                          attr ((:schema param-ctx) ident) #_"nil for db/id"]
                                      (case (-> attr :attribute/valueType :db/ident)
                                        :db.type/ref
                                        [{:anchor/prompt (str "edit") ; conserve space in label
                                          :anchor/ident :sys
                                          :anchor/repeating? true
                                          :anchor/find-element fe
                                          :anchor/attribute attr
                                          :anchor/link (link-query-system-edit-attr parent-link fe attr)}
                                         {:anchor/prompt (str "new") ; conserve space in label
                                          :anchor/ident :sys
                                          :anchor/repeating? false
                                          :anchor/find-element fe
                                          :anchor/attribute attr
                                          :anchor/render-inline? true
                                          :anchor/link (link-query-system-edit-attr parent-link fe attr)}]
                                        nil))))
                          doall)

                     :link-entity
                     (->> (partition 4 colspec)
                          (mapcat (fn [[conn fe-name ident maybe-field]]
                                    (let [attr ((:schema param-ctx) ident) #_"nil for db/id"]
                                      (case (-> attr :attribute/valueType :db/ident)
                                        :db.type/ref
                                        [{:anchor/prompt (str "edit") ; conserve space in label
                                          :anchor/ident :sys
                                          :anchor/repeating? true
                                          :anchor/find-element nil
                                          :anchor/attribute attr
                                          :anchor/link (link-entity-system-edit-attr parent-link conn attr)}
                                         {:anchor/prompt (str "new") ; conserve space in label
                                          :anchor/ident :sys
                                          :anchor/repeating? false
                                          :anchor/find-element nil
                                          :anchor/attribute attr
                                          :anchor/render-inline? true
                                          :anchor/link (link-entity-system-edit-attr parent-link conn attr)}]
                                        nil))))
                          doall)

                     :link-blank [])]
    (concat entity-links attr-links)))


(defn request-for-system-link [system-link-idmap]
  (assert (:parent-link system-link-idmap))
  ; both need parent-link
  ; both need fe
  ; one needs attr
  ; all this is in the parent-link, we can just overhydrate and then prune away what we don't need.
  (->EntityRequest (:parent-link system-link-idmap)
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
                                                   {:find-element/form form-pull-exp}]}]}])))

(defn entity-conn [param-ctx]
  (let [dbid-s (-> param-ctx :query-params :entity)]
    {:db/id (->DbId (if (vector? dbid-s)
                      (:conn-id (first dbid-s))
                      (:conn-id dbid-s))
                    nil #_"ignored in the place we need it, ->entityRequest")}))

(defn link-query-fe [fe-name parent-link]
  (first (filter #(= (:db/id %) fe-name)
                 (-> parent-link :link/request :link-query/find-element))))

(defn hydrate-system-link [system-link-idmap parent-link param-ctx]
  ; entity-links don't have a fe. Inherit the connection in this case.
  ; query-links use the dbid because it is a database dependency.
  (case (:ident system-link-idmap)
    :system-edit
    (if (:find-element system-link-idmap)
      (link-query-system-edit parent-link (link-query-fe (:find-element system-link-idmap) parent-link))
      (link-entity-system-edit parent-link {:db/id (:conn system-link-idmap)}))
    :system-edit-attr
    (let [attr {:db/id (:attribute system-link-idmap)} #_"fake pulled tree"]
      (if (:find-element system-link-idmap)
        (link-query-system-edit-attr parent-link (link-query-fe (:find-element system-link-idmap) parent-link) attr)
        (link-entity-system-edit-attr parent-link {:db/id (:conn system-link-idmap)} attr)))))
