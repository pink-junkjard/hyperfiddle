(ns hyperfiddle.io.global-basis
  (:refer-clojure :exclude [compare])
  (:require
    [bidi.bidi :as bidi]
    [cats.core :as cats :refer [mlet]]
    [cats.labs.promise]
    [clojure.set :as set]
    [contrib.performance :as perf]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.io.http :refer [build-routes]]
    [hyperfiddle.io.http.core :refer [http-request!]]
    [hyperfiddle.io.hydrate-requests :refer [hydrate-all-or-nothing!]]
    [hyperfiddle.runtime :as runtime]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(defn uris-for-domain [domain]
  (->> (:domain/databases domain)
       (map (comp :database/uri :domain.database/record))
       (cons (get-in domain [:domain/fiddle-database :database/uri]))
       (into #{})))

(defn global-basis [rt domain-eid]                          ; this is foundation code, app-fn level (Just sees configured datomic URIs, no userland api fn)
  (perf/time-promise
    (mlet [:let [domain-requests [(foundation/domain-request domain-eid rt)
                                  (foundation/domain-request [:domain/ident foundation/source-domain-ident] rt)]]
           domain-basis (runtime/sync rt #{foundation/domain-uri})
           [user-domain foundation-domain] (hydrate-all-or-nothing! rt domain-basis nil domain-requests)
           _ (if (nil? (:db/id user-domain))
               ; terminate when domain not found
               ; todo force domain hydration before global-basis
               (p/rejected (ex-info "Domain does not exist" {:hyperfiddle.io/http-status-code 404
                                                             :domain-eid domain-eid}))
               (p/resolved nil))
           :let [user-domain (foundation/process-domain user-domain)
                 ide-domain (foundation/process-domain foundation-domain)
                 user-domain-uris (uris-for-domain user-domain) ; Any reachable thing, not per route.
                 ide-domain-uris (cond
                                   ; this logic belongs in domain or ide
                                   (:active-ide? (runtime/host-env rt))
                                   (uris-for-domain ide-domain)

                                   (get-in user-domain [:domain/environment :enable-hf-live?])
                                   [(get-in ide-domain [:domain/fiddle-database :database/uri])])
                 uris (set/union user-domain-uris ide-domain-uris)]
           sync (runtime/sync rt uris)]
      (cats/return                                          ; Just return the sync and reconstruct which is what in local-basis
        #_sync
        {:domain domain-basis
         :ide (->> ide-domain-uris                          ; Not allowed structure here
                   (map (juxt identity #(get sync %)))
                   (into {}))
         :user (->> user-domain-uris
                    (map (juxt identity #(get sync %)))
                    (into {}))}))
    (fn [err get-total-time]
      (timbre/debug "global-basis failure;" "total time:" (get-total-time)))
    (fn [success get-total-time]
      (timbre/debug "global-basis;" "total time:" (get-total-time)))))

; This knows about userland api fn (but has no assumptions e.g. that it is the browser-api-fn)

(defn global-basis-rpc! [service-uri build & [jwt]]
  (-> {:url (str service-uri (bidi/path-for (build-routes build) :global-basis))
       :accept :application/transit+json :as :auto
       :method :get}
      (into (when jwt {:auth {:bearer jwt}}))
      (http-request!)
      (p/then :body)))

(def ERROR-MISMATCHED-URIS "Bases cannot be compared; mismatched uris")
(def ERROR-BOTH-GREATER-AND-LESS-THAN "Bases cannot be compared; different values are > and <")

(letfn [(zero->nil [i] (when-not (= 0 i) i))
        (compare-uri-maps [x y]
          (if-not (= (keys x) (keys y))
            (throw (ex-info ERROR-MISMATCHED-URIS {:x x :y y}))
            (reduce
              (fn [acc [xk xv]]
                (let [r (clojure.core/compare xv (get y xk))]
                  (cond
                    (= 0 acc) r
                    (= 0 r) acc
                    (not= acc r) (throw (ex-info ERROR-BOTH-GREATER-AND-LESS-THAN {:x x :y y}))
                    :else acc)))
              0
              x)))]
  (defn compare [x y]
    (cond
      (identical? x y) 0
      (nil? x) -1
      (nil? y) 1
      :else (if-let [d (zero->nil (compare-uri-maps (:domain x) (:domain y)))]
              ; compare domain first, if different, the user/ide keys might be different which is ok
              d
              (let [xm (merge (:ide x) (:user x))
                    ym (merge (:ide y) (:user y))]
                (compare-uri-maps xm ym))))))
