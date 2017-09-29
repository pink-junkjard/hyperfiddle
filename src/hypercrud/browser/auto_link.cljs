(ns hypercrud.browser.auto-link
  (:require [hypercrud.client.core :as hc]
            [hypercrud.types.DbId :refer [->DbId]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]))


(defn system-link? [link-dbid]
  (map? (:id link-dbid)))

(defn link-system-edit [fe-name uri]                  ; these need to be thick/hydrated params bc we are manufacturing a pulled tree here.
  {:pre [fe-name uri]}
  {:db/id (->DbId {:ident :system-edit
                   :fe-name fe-name
                   :uri uri}
                  hc/*root-conn-id*)
   :link/name (str "system-" fe-name)
   :request/type :entity
   :link-query/find-element [{:find-element/name "entity"
                              :find-element/connection {:dbhole/uri uri}}]})

(defn link-system-edit-attr [fe-name uri a]
  {:pre [fe-name uri a]}
  {:db/id (->DbId {:ident :system-edit-attr
                   :fe-name fe-name
                   :uri uri
                   :a a}
                  hc/*root-conn-id*)
   :link/name (str "system-" fe-name "-" a)
   :request/type :entity
   :link-query/find-element [{:find-element/name "entity"
                              :find-element/connection {:dbhole/uri uri}}]})

(defn link-blank-system-remove [fe-name a]
  {:pre []}
  {:db/id (->DbId {:ident :sys-remove
                   :fe-name fe-name
                   :a a}
                  hc/*root-conn-id*)
   :link/name "sys-remove"
   :request/type :blank
   :link/renderer (pr-str `(fn [result# ordered-fes# anchors# param-ctx#]
                             [:p "Retract entity?"]))})

(defn hydrate-system-link [{:keys [fe-name uri a ident]} param-ctx]
  (case ident
    :system-edit (link-system-edit fe-name uri)
    :system-edit-attr (link-system-edit-attr fe-name uri a)
    :sys-remove (link-blank-system-remove fe-name a)))
