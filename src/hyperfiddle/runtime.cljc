(ns hyperfiddle.runtime
  (:refer-clojure :exclude [sync]))


(defprotocol IO
  (global-basis [rt])
  (hydrate-requests [rt local-basis stage requests])
  (hydrate-route [rt branch])
  (local-basis [rt branch-ident])
  (sync [rt dbs])
  (transact! [rt tx-groups]))

(defprotocol State
  (dispatch! [rt action-or-func])
  (state [rt] [rt path]))

(defprotocol Schema
  (hydrate-schemas [rt branch]))

(defprotocol DomainRegistry
  (domain [rt]))

(defprotocol HostInfo
  (host-env [rt]))

(defrecord HostEnvironment [hostname service-uri active-ide?])

(comment
  ; selfhost HostEnvironment examples
  (defn single-domain-host [hostname]
    (map->HostEnvironment
      {:hostname hostname
       :service-uri (->URI (str hostname "/api/"))
       :active-ide? (case hostname
                      "admin.example.com" true
                      "example.com" false)}))

  (defn multi-domain-host [hostname]
    (let [active-ide? (cond
                        (string/ends-with? hostname ".admin.example.com") true
                        (string/ends-with? hostname ".apps.example.com" false))]
      (map->HostEnvironment
        {:hostname hostname
         :service-uri (->URI (str hostname "/api/"))
         :active-ide? active-ide?
         ; these 2 keys are wip
         :domain-eid [:domain/ident (second (re-find #"(.*)\.(?:apps|admin)\.example\.com" hostname))]
         :ide/root "admin.example.com"}))))
