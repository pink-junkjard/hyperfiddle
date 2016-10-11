(ns hypercrud.browser.links
  (:require [hypercrud.browser.base-64-url-safe :as base64]))


(defn entity-link [form-id entity-id]
  (str form-id "/entity/" entity-id))


(defn field-link [form-id entity-id field-ident]
  (str (entity-link form-id entity-id) "/" (base64/encode field-ident)))


(defn query-link [form-id q]
  (str form-id "/query/" (base64/encode q)))
