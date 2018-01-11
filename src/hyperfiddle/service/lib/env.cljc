(ns hyperfiddle.service.lib.env
  #?(:cljs (:require [cljs.nodejs :as node]))
  #?(:clj
     (:import [clojure.lang ILookup])))


; These are constants loaded when the process starts. Restart the process to change them.
(defn get-env []
  (let [required #{:BUILD
                   :HF_HOSTNAME
                   :AUTH0_DOMAIN
                   :AUTH0_CLIENT_ID
                   :AUTH0_CLIENT_SECRET
                   :STATIC_RESOURCES
                   :NODE_PORT}
        optional #{:ANALYTICS}
        env (let [env #?(:clj  (let [raw-env (System/getenv)]
                                 (reify
                                   ILookup
                                   (valAt [o k] (.get raw-env k))
                                   (valAt [o k not-found] (or (.get raw-env k) not-found))))
                         :cljs (let [raw-env (.-env node/process)]
                                 (reify
                                   ILookup
                                   (-lookup [o k] (aget raw-env k))
                                   (-lookup [o k not-found] (or (aget raw-env k) not-found)))))]
              (-> (->> (concat required optional)
                       (reduce (fn [acc k]
                                 (assoc acc k (get env (name k))))
                               {}))
                  ; todo this check is garbage
                  (update :ANALYTICS #(not= % "false"))))]
    (doseq [v required]
      (assert (not (nil? (get env v))) (str "Environment variable for '" v "' not found")))
    env))
