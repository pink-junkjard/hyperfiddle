(ns hypercrud.browser.links
  (:require [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.client.internal :as internal]
            [hypercrud.form.q-util :as q-util]))


(defn entity-link [form-id dbid]
  (str form-id "/entity/" (.id dbid) "/" (base64/encode (internal/transit-encode (.conn-id dbid)))))


(defn field-link [form-id entity-id field-ident]
  (str (entity-link form-id entity-id) "/" (base64/encode field-ident)))


(defn raw-query-link [form-id q param]
  (str form-id "/query/" (base64/encode {:q q :params param})))


(defn query-link [{:keys [:link/query :link/form :link/formula]} queries param-ctx]
  (let [query (get queries query)]
    (raw-query-link form (:query/value query)
                    (q-util/build-params-from-formula query formula param-ctx))))
