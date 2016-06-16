(ns hypercrud.client.reagent
  (:require [hypercrud.client.core :as hypercrud]))


(defmulti promised (fn [client promise comp & [loading-comp]] :default))


(defn entity [client eid comp & [loading-comp]]
  [promised client (hypercrud/entity* client eid) comp loading-comp])


(defn query
  ([client q comp] (query client q [] comp))
  ([client q query-params comp & [loading-comp]]
   [promised client (hypercrud/query* client q query-params) comp loading-comp]))


(defn enter [client comp & [loading-comp]]
  [promised client (hypercrud/enter* client) comp loading-comp])
