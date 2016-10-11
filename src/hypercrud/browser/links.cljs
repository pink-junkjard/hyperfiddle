(ns hypercrud.browser.links
  (:require [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.form.q-util :as q-util]))


(defn entity-link [form-id entity-id]
  (str form-id "/entity/" entity-id))


(defn field-link [form-id entity-id field-ident]
  (str (entity-link form-id entity-id) "/" (base64/encode field-ident)))


(defn query-link
  ([form-id q param]
   (str form-id "/query/" (base64/encode {:q q :params param})))
  ([query param-ctx]
   (query-link (:query/form query)
               (:query/value query)
               (q-util/build-params-from-formula query param-ctx))))
