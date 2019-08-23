(ns hyperfiddle.io.core
  #?(:clj (:refer-clojure :exclude [sync]))
  (:require
    [cats.core :as cats]
    [cats.monad.either :as either]
    [promesa.core :as p]))


(defprotocol IO
  (global-basis [io])
  (hydrate-requests [io local-basis partitions requests])
  (hydrate-route [io local-basis route pid partitions])
  (local-basis [io global-basis route])
  (sync [io dbnames])
  (transact! [io tx-groups]))

(defn hydrate-one! [io local-basis partitions request]
  (-> (hydrate-requests io local-basis partitions [request])
      (p/then (fn [{:keys [pulled-trees]}] (either/branch (first pulled-trees) p/rejected p/resolved)))))

; Promise[List[Response]]
(defn hydrate-all-or-nothing! [io local-basis partitions requests]
  (if (empty? requests)
    (p/resolved nil)
    (-> (hydrate-requests io local-basis partitions requests)
        (p/then (fn [{:keys [pulled-trees]}] (either/branch (cats/sequence pulled-trees) p/rejected p/resolved))))))
