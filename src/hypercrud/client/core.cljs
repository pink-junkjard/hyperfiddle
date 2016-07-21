(ns hypercrud.client.core)


(defprotocol Graph
  (select [this named-query])
  (entity [this eid])
  (with [this more-statements]))


(defprotocol Client
  (authenticate! [this username password])                  ; sets a http cookie with a token
  (whoami [this])                                           ; read the cookie
  (graph [this])
  (enter! [this query t])
  (transact! [this tx]))


(defn tempid!-factory []
  (let [n (atom 0)]
    (fn [] (swap! n dec) @n)))
