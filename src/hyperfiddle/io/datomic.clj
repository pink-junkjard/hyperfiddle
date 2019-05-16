(ns hyperfiddle.io.datomic)


(defprotocol DatomicFacade
  (as-of [o db time-point])
  (basis [o conn])
  (basis-t [o db])
  (connect [o hf-db])
  (db [o conn])
  (pull [o db arg-map])
  (q [o arg-map])
  (transact [o conn arg-map])
  (with [o db arg-map])
  (with-db [o conn]))
