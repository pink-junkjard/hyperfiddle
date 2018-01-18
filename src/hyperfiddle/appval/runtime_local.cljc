(ns hyperfiddle.appval.runtime-local
  (:require [cats.core :as cats :refer [mlet]]
            [cats.labs.promise]
            [clojure.set :as set]
            [cuerdas.core :as str]
            [hypercrud.util.core :refer [unwrap]]
            [hypercrud.client.peer :as peer]
            [hypercrud.client.core :as hypercrud]
            [hypercrud.util.core :as util]
            [hypercrud.util.performance :as perf]

            [hyperfiddle.appfn.runtime-local :refer [hydrate-all-or-nothing! hydrate-one! hydrate-loop]] ;todo
            [hyperfiddle.appval.domain.foundation :as foundation]
            [hyperfiddle.appval.domain.core :as foundation2]
            [hyperfiddle.appval.state.reducers :as reducers]
            [hypercrud.util.reactive :as reactive]

            [hyperfiddle.runtime :as api]
            [promesa.core :as p]
            [taoensso.timbre :as timbre]))


(defn uris-for-domain-legacy [domain]
  #_(:domain/databases domain)
  (->> (:domain/code-databases domain)                      ; browser. Lift all databases to top-level on domain.
       (map (fn [repo]
              [(:dbhole/name repo)
               (into #{(:dbhole/uri repo)}
                     (->> (:repository/environment repo)
                          (filter (fn [[k v]] (str/starts-with? k "$")))
                          vals))]))
       (into {})))

(defn global-basis [rt hyperfiddle-hostname hostname]       ; this is foundation code, app-fn level (Just sees configured datomic URIs, no userland api fn)
  (perf/time-promise
    (mlet [:let [domain-requests [(foundation2/domain-request (foundation2/hostname->hf-domain-name hostname hyperfiddle-hostname) rt)
                                  (foundation2/domain-request "hyperfiddle" rt)]]
           domain-basis (api/sync rt #{foundation2/domain-uri})
           [user-domain foundation-domain] (hydrate-all-or-nothing! rt domain-basis nil domain-requests)
           :let [user-domain (foundation/process-domain-legacy user-domain)
                 ide-domain (foundation/process-domain-legacy foundation-domain)
                 user-domain-uris (uris-for-domain-legacy user-domain) ; Any reachable thing, not per route.
                 ide-domain-uris (uris-for-domain-legacy ide-domain)
                 uris (apply set/union (concat (vals user-domain-uris)
                                               (vals ide-domain-uris)))]
           sync (api/sync rt uris)]
      (cats/return                                          ; Just return the sync and reconstruct which is what in local-basis
        #_sync
        {:domain domain-basis
         :ide (->> ide-domain-uris                          ; Not allowed structure here
                   (util/map-values (fn [repo-uris]
                                      (->> repo-uris
                                           (map (juxt identity #(get sync %)))
                                           (into {})))))
         :user (->> user-domain-uris                        ; Not allowed structure here
                    (util/map-values (fn [repo-uris]
                                       (->> repo-uris
                                            (map (juxt identity #(get sync %)))
                                            (into {})))))}))
    (fn [err get-total-time]
      (timbre/debug "global-basis failure;" "total time:" (get-total-time)))
    (fn [success get-total-time]
      (timbre/debug "global-basis;" "total time:" (get-total-time)))))

; This knows about userland api fn (but has no assumptions e.g. that it is the browser-api-fn)

(comment
  (def global-basis {#uri"datomic:free://datomic:4334/domains" 1316
                     #uri"datomic:free://datomic:4334/root" 16754,
                     #uri"datomic:free://datomic:4334/kalzumeus" 1037,
                     #uri"datomic:free://datomic:4334/hyperblog" 1115})

  (= (local-basis _ _ global-basis _ {:type "page"})
     {#uri"datomic:free://datomic:4334/domains" 1316,
      #uri"datomic:free://datomic:4334/hyperblog" 1115,
      #uri"datomic:free://datomic:4334/kalzumeus" 1037,
      #uri"datomic:free://datomic:4334/root" 16754})

  (= (local-basis _ _ global-basis _ {:type "ide"})
     {#uri"datomic:free://datomic:4334/domains" 1316,
      #uri"datomic:free://datomic:4334/hyperblog" 1115,
      #uri"datomic:free://datomic:4334/root" 16754})

  (= (local-basis _ _ global-basis _ {:type "user"})
     {#uri"datomic:free://datomic:4334/domains" 1316,
      #uri"datomic:free://datomic:4334/hyperblog" 1115,
      #uri"datomic:free://datomic:4334/kalzumeus" 1037}))

(defn hydrate-loop-adapter [local-basis stage ctx ->Runtime f]
  ; Hacks because the hydrate-loop doesn't write to the state atom.
  (fn [id->tempid ptm]
    (let [state-val {:local-basis local-basis
                     :tempid-lookups id->tempid
                     :ptm ptm
                     :stage stage}
          ctx (assoc ctx :peer (->Runtime (reducers/root-reducer state-val nil)))]
      (f ctx))))
