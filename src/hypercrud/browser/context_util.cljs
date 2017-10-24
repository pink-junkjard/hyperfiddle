(ns hypercrud.browser.context-util)


(defn- ident->uri [ident dbholes]
  (->> dbholes
       (filter #(= (:dbhole/name %) ident))
       first
       :dbhole/uri))

(defn code-ident->database-uri [ident ctx]
  (->> (get-in ctx [:domain :domain/code-databases])
       (ident->uri ident)))

(defn ident->database-uri [ident ctx]
  (->> (get-in ctx [:domain :domain/databases])
       (ident->uri ident)))
