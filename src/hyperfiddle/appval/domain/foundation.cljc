(ns hyperfiddle.appval.domain.foundation
  (:require [cats.monad.either :as either]
            [hypercrud.client.peer :as peer]
            [hypercrud.util.exception :refer [->Exception]]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.compile.reader :as reader]
            [hypercrud.util.string :as hc-string]

            [hypercrud.client.core :as hc]
            [hyperfiddle.appval.domain.core :as hf]))



; Can be removed once domain/databases are flattened up.
(defn process-domain-legacy [domain]
  (-> (into {} domain)
      (update :domain/code-databases
              (fn [repos]
                (->> repos
                     (map (fn [repo]
                            (-> (into {} repo)
                                ; todo this can throw
                                (update :repository/environment reader/read-string)))))))))

(defn ->decoded-route [maybe-decoded-route target-domain]
  (or maybe-decoded-route
      (-> (hc-string/safe-read-edn-string (:domain/home-route target-domain))
          (either/branch (constantly nil) identity))))

(defn local-basis [global-basis domain route]
  (:domain global-basis))

; Foundation needs to give domain, and thats it i guess
(defn api [foo user-api-fn hyperfiddle-hostname hostname branch state-val]
  (let [ctx {:branch branch
             :dispatch! #(throw (->Exception "dispatch! not supported in hydrate-route"))
             :hostname hostname
             :hyperfiddle-hostname hyperfiddle-hostname
             ; just blast the peer everytime
             :peer (peer/->Peer (reactive/atom state-val))
             :peer2 (peer/->ApiPeer (reactive/atom state-val))}
        target-domain-api (let [hf-domain-name (hf/hostname->hf-domain-name (:hostname ctx) (:hyperfiddle-hostname ctx))]
                                (hf/domain-request hf-domain-name (:peer ctx)))
        target-domain (hc/hydrate (:peer2 ctx) target-domain-api)]
    ; Always need target-domain, but that's about all we know, because IDE popovers.
    (concat [target-domain-api]
            (user-api-fn target-domain foo state-val ctx))))
