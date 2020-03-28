(ns hyperfiddle.domains.transit
  (:require
    [cognitect.transit :as t]
    [hypercrud.transit :as hc-t]))


(def ^:private read-handlers (atom hc-t/read-handlers))
(def ^:private write-handlers (atom hc-t/write-handlers))

(defn register-handlers [c tag rep-fn from-rep]
  (swap! read-handlers assoc tag (t/read-handler from-rep))
  (swap! write-handlers assoc c (t/write-handler (constantly tag) rep-fn)))

(defn decode
  ([s]
   (hc-t/decode s :opts {:handlers @read-handlers}))
  ([s type]
   (hc-t/decode s :opts {:handlers @read-handlers} :type type)))

(defn encode
  ([x]
   (hc-t/encode x :opts {:handlers @write-handlers}))
  ([x type]
   (hc-t/encode x :opts {:handlers @write-handlers} :type type)))
