(ns hyperfiddle.api
  (:require
    [hyperfiddle.tempid :refer [with-tempid-color tempid-from-ctx tempid-from-stage]]))


(defn ^:export tempid-child "stable and idempotent, but implemented through parent-child ctx"
  [ctx val]
  (with-tempid-color ctx tempid-from-ctx))

; This returns a new value each time the transaction changes - can't call it again later.
; So tx-fns must inspect the modal-route, they can't re-create the dbid.
(defn ^:export tempid-detached "unstable but guaranteed unique tempid"
  ([dbname ctx]
   (with-tempid-color dbname ctx (partial tempid-from-stage dbname)))
  ([ctx]
   (with-tempid-color ctx tempid-from-stage)))
