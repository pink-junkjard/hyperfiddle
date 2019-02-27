(ns hyperfiddle.io.core
  #?(:clj (:refer-clojure :exclude [sync]))
  (:require
    [cats.core :as cats]
    [cats.monad.either :as either]
    [promesa.core :as p]))


(defprotocol IO
  (global-basis [io])
  (hydrate-requests [io local-basis staged-branches requests]) ; staged-branches :: List[Map[keyword, Any]]
  (hydrate-route [io local-basis route branch stage])       ; stage :: Map[branch-ident, multi-color-tx] ; multi-color-tx :: Map[dbname, tx]
  (local-basis [io global-basis route])
  (sync [io dbnames])
  (transact! [io tx-groups]))

(defn hydrate-one! [io local-basis staged-branches request]
  (-> (hydrate-requests io local-basis staged-branches [request])
      (p/then (fn [{:keys [pulled-trees]}] (either/branch (first pulled-trees) p/rejected p/resolved)))))

; Promise[List[Response]]
(defn hydrate-all-or-nothing! [io local-basis staged-branches requests]
  (if (empty? requests)
    (p/resolved nil)
    (-> (hydrate-requests io local-basis staged-branches requests)
        (p/then (fn [{:keys [pulled-trees]}] (either/branch (cats/sequence pulled-trees) p/rejected p/resolved))))))
