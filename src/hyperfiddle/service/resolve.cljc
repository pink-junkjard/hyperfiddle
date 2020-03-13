(ns hyperfiddle.service.resolve)

(def mode
  (if (not (some-> (resolve 'user/dev-mode) deref))
    :prod
    :dev))

(def dev
  (= mode :dev))

(def api-version-tag "0.0.1")

(def assets {:mode #{:web :shadow}
             :path "./.serve"
             :url  "/static/dev"})


(defprotocol HF-Resolve
  :extend-via-metadata true

  (setup [R])
  (handle [R request])

  (attr [R topic ks])
  (set-attr [R topic ks val])

  (uri-for [R topic location])
  (request [R topic])
  (dispatch [R topic])

  (render [R topic])
  (serve [R topic])
  (IO [R topic])
  (run-IO [R topic f]))

;(defprotocol HF-Resolve-In) ; Todo

(defprotocol Resolve-From
  :extend-via-metadata true
  (from [topic]))

(defn via [task f & args]
  (apply f (from task) task args))

(defn assoc-with [val R]
  (vary-meta val
    (fn [M]
      (apply merge
        (or M {})
        {`from (fn R-from [& _] R)}
        (for [method (->> HF-Resolve :method-builders keys)]
          {(symbol method) (fn R-proxy [_ & args] (apply @method R args))})))))
