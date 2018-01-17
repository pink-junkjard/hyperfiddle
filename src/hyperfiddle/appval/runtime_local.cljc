(ns hyperfiddle.appval.runtime-local
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [cats.labs.promise]
            [clojure.set :as set]
            [cuerdas.core :as str]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.peer :as peer]
            [hypercrud.util.core :as util]
            [hypercrud.util.exception :refer [->Exception]]
            [hypercrud.util.non-fatal :refer [try-either]]
            [hypercrud.util.performance :as perf]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.util.string :as hc-string]
            [hyperfiddle.appfn.runtime-local :refer [hydrate-all-or-nothing! hydrate-one! hydrate-loop]] ;todo
            [hyperfiddle.appval.domain.app :as ide]
            [hyperfiddle.appval.domain.core :as hf]
            [hyperfiddle.appval.state.reducers :as reducers]
            [hyperfiddle.api :as api]
            [promesa.core :as p]
            [taoensso.timbre :as timbre]))


(defn global-basis [rt hyperfiddle-hostname hostname]
  (perf/time-promise
    (mlet [:let [uris-for-domain (fn [domain]
                                   (->> (:domain/code-databases domain)
                                        (map (fn [repo]
                                               [(:dbhole/name repo)
                                                (into #{(:dbhole/uri repo)}
                                                      (->> (:repository/environment repo)
                                                           (filter (fn [[k v]] (str/starts-with? k "$")))
                                                           vals))]))
                                        (into {})))
                 domain-requests [(hf/domain-request (hf/hostname->hf-domain-name hostname hyperfiddle-hostname) rt)
                                  (hf/domain-request "hyperfiddle" rt)]]
           domain-basis (api/sync rt #{hf/domain-uri})
           [user-domain hf-domain] (hydrate-all-or-nothing! rt domain-basis nil domain-requests)
           :let [user-domain (ide/process-domain user-domain)
                 hf-domain (ide/process-domain hf-domain)
                 user-domain-uris (uris-for-domain user-domain) ; Any reachable thing, not per route.
                 hf-domain-uris (uris-for-domain hf-domain)
                 uris (apply set/union (concat (vals user-domain-uris)
                                               (vals hf-domain-uris)))]
           sync (api/sync rt uris)]
      (cats/return
        {:domain domain-basis
         :ide (->> hf-domain-uris
                   (util/map-values (fn [repo-uris]
                                      (->> repo-uris
                                           (map (juxt identity #(get sync %)))
                                           (into {})))))
         :user (->> user-domain-uris
                    (util/map-values (fn [repo-uris]
                                       (->> repo-uris
                                            (map (juxt identity #(get sync %)))
                                            (into {})))))}))
    (fn [err get-total-time]
      (timbre/debug "global-basis failure;" "total time:" (get-total-time)))
    (fn [success get-total-time]
      (timbre/debug "global-basis;" "total time:" (get-total-time)))))

(defn local-basis [rt hyperfiddle-hostname hostname global-basis encoded-route foo]
  (perf/time-promise
    (mlet [maybe-decoded-route (if encoded-route
                                 (either/branch (try-either (routing/decode encoded-route)) p/rejected p/resolved)
                                 (p/resolved nil))
           user-domain (let [domain-basis (apply sorted-map (apply concat (:domain global-basis)))
                             user-domain-request (hf/domain-request (hf/hostname->hf-domain-name hostname hyperfiddle-hostname) rt)]
                         (-> (hydrate-one! rt domain-basis nil user-domain-request)
                             (p/then ide/process-domain)))
           route (or maybe-decoded-route
                     (-> (hc-string/safe-read-edn-string (:domain/home-route user-domain))
                         (either/branch p/rejected p/resolved)))
           :let [{:keys [domain ide user]} global-basis
                 ; basis-maps: List[Map[uri, t]]
                 basis-maps (condp = (:type foo)
                              ; everybody has purple - even the IDE's new-anchor popover needs to do a subdomain lookup on the server side
                              "page" (concat (vals ide)
                                             (vals user)
                                             [domain])

                              "ide" (conj (vals ide)
                                          ; code-database from foo not route
                                          (get user (:code-database foo)) ; todo the only repo uri is needed from user. dont need the environment as well
                                          domain)

                              "user" [domain (get user (:code-database route))])
                 local-basis (->> basis-maps
                                  (apply concat)
                                  (apply concat)
                                  (apply sorted-map))]
           ; Local-basis is for the subset of databases visible now for this fiddle.
           ; Does not include popovers or navigates, they have their own local basis.
           ; In the future, we can do better than reachable-basis if we hydrate and see what came out.
           #_(hydrate-route rt hostname foo state-val)
           #_(determine-local-basis)]
      (cats/return local-basis))
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

(defn hydrate-route [rt hyperfiddle-hostname hostname local-basis encoded-route foo branch stage & [data-cache]]
  (let [request-fn (let [ctx {:branch branch
                              :dispatch! #(throw (->Exception "dispatch! not supported in hydrate-route"))
                              :foo foo
                              :hostname hostname
                              :hyperfiddle-hostname hyperfiddle-hostname}]
                     (fn [id->tempid ptm]
                       (let [state-val (-> {:encoded-route encoded-route
                                            :local-basis local-basis
                                            :tempid-lookups id->tempid
                                            :ptm ptm
                                            :stage stage}
                                           (reducers/root-reducer nil))
                             ; just blast the peer everytime
                             ctx (assoc ctx :peer (peer/->Peer (reactive/atom state-val)))]
                         (case (:type foo)
                           "page" (ide/page-request state-val ctx)
                           "ide" (ide/ide-request state-val ctx)
                           "user" (ide/user-request state-val ctx)))))]
    (hydrate-loop rt request-fn local-basis stage data-cache)))
