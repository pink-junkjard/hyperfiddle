(ns hypercrud.browser.routing
  (:require
    [cats.core :as cats :refer [mlet]]
    [cats.monad.either :as either]
    [clojure.set :as set]
    [clojure.walk :as walk]
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [xorxs]]
    [contrib.eval :as eval]
    [contrib.reactive :as r]
    [contrib.string :refer [memoized-safe-read-edn-string]]
    [contrib.try$ :refer [try-either]]
    [cuerdas.core :as string]
    [hypercrud.browser.q-util :as q-util]
    [hypercrud.browser.router :as router]
    [hypercrud.types.Entity :refer [#?(:cljs Entity)]]
    [hypercrud.types.ThinEntity :refer [->ThinEntity #?(:cljs ThinEntity)]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]
    [taoensso.timbre :as timbre])
  #?(:clj
     (:import (hypercrud.types.Entity Entity)
              (hypercrud.types.ThinEntity ThinEntity))))


(defn invert-route [domain [_ args :as route] invert-id]
  (let [args (->> {:request-params args}                    ; code compat
                  (walk/postwalk (fn [v]                    ; works on [args] instead of (:request-param args) ?
                                   (cond
                                     (instance? Entity v) (assert false "hyperfiddle/hyperfiddle.net#150")

                                     (instance? ThinEntity v)
                                     (let [uri (domain/dbname->uri (.-dbname v) domain)
                                           id (invert-id (.-id v) uri)]
                                       (->ThinEntity (.-dbname v) id))

                                     :else v)))
                  :request-params)]
    (assoc route 1 args)))

(defn ctx->id-lookup [uri ctx]
  ; todo what about if the tempid is on a higher branch in the uri?
  @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :tempid-lookups uri]))

(defn id->tempid [route ctx]
  (let [invert-id (fn [id uri]
                    (let [id->tempid (ctx->id-lookup uri ctx)]
                      (get id->tempid id id)))]
    (invert-route (:hypercrud.browser/domain ctx) route invert-id)))

(defn tempid->id [route ctx]
  (let [invert-id (fn [temp-id uri]
                    (let [tempid->id (-> (ctx->id-lookup uri ctx)
                                         (set/map-invert))]
                      (get tempid->id temp-id temp-id)))]
    (invert-route (:hypercrud.browser/domain ctx) route invert-id)))

(defn route [ctx route]                                     ; circular, this can be done sooner
  {:pre [(if-let [params (second route)] (vector? params) true) ; validate normalized already
         (some-> ctx :hypercrud.browser/domain :domain/fiddle-database :database/uri)]}
  (assoc ctx :route (tempid->id route ctx)))

(defn normalize-args [porps]
  {:pre [(not (:entity porps)) #_"legacy"
         ; There is some weird shit hitting this assert, like {:db/id nil}
         #_(not (map? porps)) #_"legacy"]
   :post [(vector? %) #_"route args are associative by position"]}

  ; careful here -
  ; (seq [1]) - truthy
  ; (seq? [1]) - false
  ; (seq 1) - IllegalArgumentException
  (cond (instance? ThinEntity porps) [porps]                ; entity is also a map so do this first
        :else (vec (xorxs porps))))

(let [memoized-eval-string (memoize eval/safe-eval-string)]
  (defn ^:export build-route' [link ctx]
    (mlet [fiddle-id (if-let [page (:link/fiddle link)]
                       (either/right (:fiddle/ident page))
                       (either/left {:message "link has no fiddle" :data {:link link}}))
           formula (let [formula-str (:link/formula link)]
                     (if (and (string? formula-str) (not (string/blank? formula-str)))
                       (memoized-eval-string formula-str)
                       (either/right (constantly nil))))
           args (try-either
                  (->> {:remove-this-wrapper @(r/track formula ctx)} ; walk trees wants a map
                       ; shadow-links can be hdyrated here, and we need to talk them.
                       ; Technical debt. Once shadow-links are identities, this is just a mapv.
                       (walk/postwalk (fn [v]
                                        (if (instance? Entity v)
                                          (let [dbname (some-> v .-uri (domain/uri->dbname (:hypercrud.browser/domain ctx)))]
                                            (->ThinEntity dbname (or (:db/ident v) (:db/id v))))
                                          v)))
                       (into {})
                       :remove-this-wrapper
                       normalize-args))]
      (cats/return
        (id->tempid (router/canonicalize fiddle-id args) ctx)))))

(defn validated-route' [fiddle route ctx]
  ; We specifically hydrate this deep just so we can validate anchors like this.
  (let [[_ [$1 :as params]] route]
    (case (:fiddle/type fiddle)
      :query (let [q (unwrap                                ; todo whats the point of this monad?
                       #(timbre/warn %)
                       (mlet [q (memoized-safe-read-edn-string (:fiddle/query fiddle))]
                         (if (vector? q)
                           (cats/return q)
                           (either/left {:message (str "Invalid query '" (pr-str q) "', only vectors supported")}))))]
               (q-util/validate-query-params q params ctx))
      :entity (if (not= nil $1)
                ; todo check conn
                (either/right route)
                (either/left {:message "malformed entity param" :data {:params params}}))
      ; nil means :blank
      (either/right route))))

(def encode router/encode)
(def decode router/decode)

(defn decode' [route]
  (if route
    (try-either (decode route))
    (either/right nil)))
