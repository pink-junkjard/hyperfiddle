(ns hyperfiddle.service.lib.env
  (:require
    #?(:cljs [cljs.nodejs :as node])
    [clojure.string :as string]
    [contrib.data :refer [update-existing]]
    #?(:cljs [goog.object :as object]))
  #?(:clj
     (:import [clojure.lang ILookup])))


; These are constants loaded when the process starts. Restart the process to change them.
(defn get-env []
  (let [required #{:BUILD
                   :HF_HOSTNAMES
                   :AUTH0_DOMAIN
                   :AUTH0_CLIENT_ID
                   :AUTH0_CLIENT_SECRET
                   :SERVICE_HOST
                   :SERVICE_PORT
                   :STATIC_RESOURCES
                   :NODE_PORT}
        optional #{:ANALYTICS :HF_ALIAS_HOSTNAMES :SERVICE_HOST}
        env (let [env #?(:clj  (let [raw-env (System/getenv)]
                                 (reify
                                   ILookup
                                   (valAt [o k] (.get raw-env k))
                                   (valAt [o k not-found] (or (.get raw-env k) not-found))))
                         :cljs (let [raw-env (.-env node/process)]
                                 (reify
                                   ILookup
                                   (-lookup [o k] (object/get raw-env k))
                                   (-lookup [o k not-found] (object/get raw-env k not-found)))))]
              (-> (->> (concat required optional)
                       (reduce (fn [acc k]
                                 (assoc acc k (get env (name k))))
                               {}))
                  (update :ANALYTICS #(not= % "false"))     ; todo this check is garbage
                  (update :HF_HOSTNAMES #(string/split % #";"))
                  (update :SERVICE_PORT #(Integer/parseInt %))
                  (update-existing :HF_ALIAS_HOSTNAMES #(string/split % #";"))))]
    (doseq [v required]
      (assert (not (nil? (get env v))) (str "Environment variable for '" v "' not found")))
    env))
