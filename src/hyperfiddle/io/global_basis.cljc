(ns hyperfiddle.io.global-basis
  (:require [cats.core :as cats :refer [mlet]]
            [cats.labs.promise]
            [clojure.set :as set]
            [cuerdas.core :as str]
            [hypercrud.http.core :refer [http-request!]]
            [hypercrud.util.core :as util]
            [hypercrud.util.performance :as perf]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-all-or-nothing!]]
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
    (mlet [:let [domain-requests [(foundation/domain-request (foundation/hostname->hf-domain-name hostname hyperfiddle-hostname) rt)
                                  (foundation/domain-request "hyperfiddle" rt)]]
           domain-basis (api/sync rt #{foundation/domain-uri})
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

(defn global-basis-rpc! [service-uri]
  (-> (http-request! {:url (str service-uri "global-basis")
                      :accept :application/transit+json :as :auto
                      :method :get})
      (p/then :body)))
