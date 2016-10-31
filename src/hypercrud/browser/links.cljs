(ns hypercrud.browser.links
  (:require [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.client.internal :as internal]
            [hypercrud.form.q-util :as q-util]))


(defn entity-link [form-dbid dbid]
  ;; form-dbid is assumed to be the editor-graph connection
  (str (.-id form-dbid) "/entity/" (.-id dbid) "/" (base64/encode (internal/transit-encode (.-conn-id dbid)))))


;(str (entity-link form-dbid entity-dbid) "/" (.-id field-dbid))
(defn field-link [field-dbid dbid]
  ;; field-dbid is assumed to be the editor-graph connection
  (str (.-id field-dbid) "/field/" (.-id dbid) "/" (base64/encode (internal/transit-encode (.-conn-id dbid)))))


(defn query-link [{:keys [:link/query :link/form :link/formula]} param-ctx]
  ;; form-dbid and query-dbid is assumed to be the editor-graph connection
  ;; the query results can be across any db, so no conn-id here
  (str (-> form .-dbid .-id) "/query/" (-> query .-dbid .-id) "/" (base64/encode (q-util/build-params-from-formula query formula param-ctx))))
