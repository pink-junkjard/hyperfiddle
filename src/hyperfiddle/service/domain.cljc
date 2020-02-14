(ns hyperfiddle.service.domain)


(defmulti route (fn [domain & args] (type domain)))
