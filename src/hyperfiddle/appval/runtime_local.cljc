(ns hyperfiddle.appval.runtime-local
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [cats.labs.promise]
            [clojure.set :as set]
            [cuerdas.core :as str]
            [hypercrud.browser.routing :as routing]
            [hypercrud.util.core :as util]
            [hypercrud.util.exception :refer [->Exception]]
            [hypercrud.util.non-fatal :refer [try-either]]
            [hypercrud.util.performance :as perf]
            [hypercrud.util.string :as hc-string]
            [hyperfiddle.appfn.runtime-local :refer [hydrate-all-or-nothing! hydrate-one! hydrate-loop]] ;todo
            [hyperfiddle.appval.domain.foundation :as foundation]
            [hyperfiddle.appval.domain.core :as hf]
            [hyperfiddle.appval.state.reducers :as reducers]

            [hyperfiddle.runtime :as api]
            [promesa.core :as p]
            [taoensso.timbre :as timbre]))


(defn ide-uris-for-domain [domain]
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
    (mlet [:let [domain-requests [(hf/domain-request (hf/hostname->hf-domain-name hostname hyperfiddle-hostname) rt)
                                  (hf/domain-request "hyperfiddle" rt)]]
           domain-basis (api/sync rt #{hf/domain-uri})
           [user-domain foundation-domain] (hydrate-all-or-nothing! rt domain-basis nil domain-requests)
           :let [user-domain (foundation/process-domain user-domain)
                 hf-domain (foundation/process-domain foundation-domain)
                 user-domain-uris (ide-uris-for-domain user-domain) ; Any reachable thing, not per route.
                 hf-domain-uris (ide-uris-for-domain hf-domain)
                 uris (apply set/union (concat (vals user-domain-uris)
                                               (vals hf-domain-uris)))]
           sync (api/sync rt uris)]
      (cats/return                                          ; Just return the sync and reconstruct which is what in local-basis
        {:domain domain-basis
         :ide (->> hf-domain-uris                           ; Not allowed structure here
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
(defn local-basis [rt user-local-basis hyperfiddle-hostname hostname global-basis encoded-route]
  (perf/time-promise
    (mlet [maybe-decoded-route (if encoded-route
                                 (either/branch (try-either (routing/decode encoded-route)) p/rejected p/resolved)
                                 (p/resolved nil))
           user-domain (let [domain-basis (apply sorted-map (apply concat (:domain global-basis)))
                             user-domain-request (hf/domain-request (hf/hostname->hf-domain-name hostname hyperfiddle-hostname) rt)]
                         (-> (hydrate-one! rt domain-basis nil user-domain-request)
                             (p/then foundation/process-domain)))
           route (or maybe-decoded-route
                     (-> (hc-string/safe-read-edn-string (:domain/home-route user-domain))
                         (either/branch p/rejected p/resolved)))]
      (cats/return (concat [user-domain] (user-local-basis global-basis user-domain route))))
    (fn [err get-total-time]
      (timbre/debug "local-basis failure;" "total time:" (get-total-time)))
    (fn [success get-total-time]
      (timbre/debug "local-basis;" "total time:" (get-total-time)))))

(comment
  (def global-basis {:domain {#uri"datomic:free://datomic:4334/domains" 1316},
                     :ide {"root" {#uri"datomic:free://datomic:4334/root" 16754,
                                   #uri"datomic:free://datomic:4334/domains" 1316}},
                     :user {"hyperblog" {#uri"datomic:free://datomic:4334/kalzumeus" 1037,
                                         #uri"datomic:free://datomic:4334/hyperblog" 1115}}})

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

(defn hydrate-route [rt user-data-fn hyperfiddle-hostname hostname
                     local-basis encoded-route branch stage & [data-cache]] ; -> DataCache
  (let [request-fn (fn [id->tempid ptm]
                     (let [state-val (-> {:encoded-route encoded-route
                                          :local-basis local-basis
                                          :tempid-lookups id->tempid
                                          :ptm ptm
                                          :stage stage}
                                         (reducers/root-reducer nil))]
                       ; No ctx yet, might not be a browser
                       (user-data-fn hyperfiddle-hostname hostname branch state-val)))]
    (hydrate-loop rt request-fn local-basis stage data-cache)))
