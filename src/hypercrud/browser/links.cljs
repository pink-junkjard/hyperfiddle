(ns hypercrud.browser.links
  (:require [cljs.reader :as reader]
            [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.client.internal :as internal]
            [hypercrud.form.q-util :as q-util]))


(defn entity-link [form-dbid dbid]
  ;; form-dbid is assumed to be the editor-graph connection
  (str (.-id form-dbid) "/entity/" (.-id dbid) "/" (base64/encode (internal/transit-encode (.-conn-id dbid)))))


;(str (entity-link form-dbid entity-dbid) "/" (.-id field-dbid))
(defn field-link [field-dbid dbid]
  ;; field-dbid is assumed to be the editor-graph connection
  (str (.-id field-dbid) "/field/" (.-id dbid) "/" (base64/encode (internal/transit-encode (.-conn-id dbid)))))


(defn raw-query-link [form-id query-id params]
  (str form-id "/query/" query-id "/" (base64/encode params)))


(defn query-link [{:keys [:link/query :link/form :link/formula]} param-ctx]
  ;; form-dbid and query-dbid is assumed to be the editor-graph connection
  ;; the query results can be across any db, so no conn-id here
  (raw-query-link (-> form :db/id .-id) (-> query :db/id .-id)
                  (q-util/build-params-from-formula query formula param-ctx)))
