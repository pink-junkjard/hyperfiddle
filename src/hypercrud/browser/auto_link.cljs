(ns hypercrud.browser.auto-link
  (:require [hypercrud.client.core :as hc]
            [hypercrud.types.DbId :refer [->DbId]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]))


(defn system-link? [link-dbid]
  (map? (:id link-dbid)))

(defn link-system-edit [owner fe]                           ; these need to be thick/hydrated params bc we are manufacturing a pulled tree here.
  {:pre [fe]}
  (let [conn (:find-element/connection fe)]
    {:db/id (->DbId {:ident :system-edit
                     :owner (-> owner :db/id :id)
                     :fe (-> fe :db/id :id)}
                    (-> conn :db/id :id))                   ; this should be root-conn, bug
     ;:hypercrud/owner owner
     :link/name (str "system-" (:find-element/name fe))
     :request/type :entity
     :link-query/find-element [{:find-element/name "entity"
                                :find-element/connection conn}]}))

(defn link-system-edit-attr [owner fe a]
  {:pre [fe a]}
  (let [conn (:find-element/connection fe)]
    {:db/id (->DbId {:ident :system-edit-attr
                     :owner (-> owner :db/id :id)
                     :fe (-> fe :db/id :id)
                     :a a}
                    (-> conn :db/id :id))
     :link/name (str "system-" (:find-element/name fe) "-" a)
     ;:hypercrud/owner owner
     :request/type :entity
     :link-query/find-element [{:find-element/name "entity"
                                :find-element/connection conn}]}))

(defn link-blank-system-remove [owner fe a]
  {:pre []}
  {:db/id (->DbId {:ident :sys-remove
                   :fe (-> fe :db/id :id)
                   :a a} nil)
   :link/name "sys-remove"
   ;:hypercrud/owner owner
   :request/type :blank
   :link/renderer (pr-str `(fn [result# ordered-fes# anchors# param-ctx#]
                             [:p "Retract entity?"]))})

(defn request-for-system-link [system-link-idmap param-ctx]
  ; owner, fe, attr.

  ; all this is in the parent-link, we can just overhydrate and then prune away what we don't need.
  ; That was the plan, but it doesn't work in second layer deep sys links, the parent is a sys link. So we
  ; need to just hydrate what we need.
  (let [dbval (hc/db (:peer param-ctx) hc/*root-conn-id* (:branch param-ctx))]
    [(->EntityRequest (->DbId (:owner system-link-idmap) hc/*root-conn-id*) nil dbval ['*])
     (if-let [fe (:fe system-link-idmap)] (->EntityRequest (->DbId fe hc/*root-conn-id*) nil dbval [:db/id :find-element/name
                                                                                                    {:find-element/connection ['*]}]))]))

(defn hydrate-system-link [system-link-idmap [owner fe] param-ctx]
  (let [owner-id (:owner system-link-idmap)
        fe-id (:fe system-link-idmap)
        a (:a system-link-idmap)]

    (case (:ident system-link-idmap)
      :system-edit (link-system-edit owner fe)
      :system-edit-attr (link-system-edit-attr owner fe a)
      :sys-remove (link-blank-system-remove owner fe a))))
