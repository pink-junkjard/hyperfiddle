(ns hypercrud.browser.routing
  (:require
    [cats.core :as cats :refer [mlet]]
    [cats.monad.either :as either :refer [left right]]
    [clojure.set :as set]
    [clojure.walk :as walk]
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [xorxs]]
    [hyperfiddle.tempid :refer [smart-identity]]
    [contrib.eval :as eval]
    [contrib.pprint :refer [pprint-str]]
    [contrib.reactive :as r]
    [contrib.string :refer [memoized-safe-read-edn-string blank->nil]]
    [contrib.try$ :refer [try-either]]
    [hypercrud.browser.q-util :as q-util]
    [hypercrud.browser.router :as router]
    [hypercrud.types.ThinEntity :refer [->ThinEntity #?(:cljs ThinEntity)]]
    [hyperfiddle.domain :as domain]
    [taoensso.timbre :as timbre]
    [hypercrud.browser.context :as context])
  #?(:clj
     (:import (hypercrud.types.ThinEntity ThinEntity))))


(defn invert-route [domain [_ args :as route] invert-id]
  (let [args (->> {:request-params args}                    ; code compat
                  (walk/postwalk (fn [v]                    ; works on [args] instead of (:request-param args) ?
                                   (if (instance? ThinEntity v)
                                     (let [uri (domain/dbname->uri (.-dbname v) domain)
                                           id (invert-id (.-id v) uri)]
                                       (->ThinEntity (.-dbname v) id))
                                     v)))
                  :request-params)]
    (assoc route 1 args)))

(defn id->tempid [route ctx]
  (let [invert-id (fn [id uri]
                    (let [id->tempid (context/ctx->id-lookup uri ctx)]
                      (get id->tempid id id)))]
    (invert-route (:hypercrud.browser/domain ctx) route invert-id)))

(defn tempid->id [route ctx]
  (let [invert-id (fn [temp-id uri]
                    (let [tempid->id (-> (context/ctx->id-lookup uri ctx)
                                         (set/map-invert))]
                      (get tempid->id temp-id temp-id)))]
    (invert-route (:hypercrud.browser/domain ctx) route invert-id)))

(defn route [ctx route]                                     ; circular, this can be done sooner
  {:pre [(if-let [params (second route)] (vector? params) true) ; validate normalized already
         (some-> ctx :hypercrud.browser/domain :domain/fiddle-database :database/uri)]}
  (assoc ctx :route (tempid->id route ctx)))

(defn validated-route+ [fiddle route ctx]
  {:pre [route]}
  ; We specifically hydrate this deep just so we can validate anchors like this.
  (let [[_ [$1 :as params]] route]
    (case (:fiddle/type fiddle)
      :query (let [q (unwrap                                ; todo whats the point of this monad?
                       #(timbre/warn %)
                       (mlet [q (memoized-safe-read-edn-string (:fiddle/query fiddle))]
                         (if (vector? q)
                           (cats/return q)
                           (either/left {:message (str "Invalid query '" (pr-str q) "', only vectors supported")}))))]
               (mlet [q-params (q-util/validate-query-params+ q params ctx)]
                 ; Ignore the params, just care that they validated.
                 (right route)))
      :entity (if (not= nil $1)                             ; todo check conn
                (right route)
                (left {:message "malformed entity param" :data {:params params}}))
      ; nil means :blank
      (either/right route))))

(defn build-link-props [+route ctx props]                   ; todo this function needs untangling; iframe ignores most of this
  ; this is a fine place to eval, put error message in the tooltip prop
  ; each prop might have special rules about his default, for example :visible is default true, does this get handled here?
  (let [[_ args :as ?route] (unwrap (constantly nil) +route)
        errors (->> [+route] (filter either/left?) (map cats/extract) (into #{}))]
    ; doesn't handle tx-fn - meant for the self-link. Weird and prob bad.
    {:route ?route                                          ; nil means no popover body
     :tooltip (if-not (empty? errors)
                [:warning (pprint-str errors)]
                (if (:hyperfiddle.ui/debug-tooltips ctx)
                  [nil (pr-str args)]
                  (:tooltip props)))
     :class (->> [(if-not (empty? errors) "invalid")]
                 (remove nil?)
                 (interpose " ")
                 (apply str))}))

(defn normalize-args [porps]
  ; There is some weird shit hitting this assert, like {:db/id nil}
  {:pre [#_(not (map? porps)) #_"legacy"]
   :post [(vector? %) #_"route args are associative by position"]}
  (vec (xorxs porps)))

(defn pull->colored-eid [ctx v]
  ; Returns a datomic primitive suitable for passing to :in. In edge cases, this could be a composite.
  (if-not (context/dbname ctx)
    v                                                       ; colorless opaque value, don't interpret as entity
    (if (map? v)
      (->ThinEntity (context/dbname ctx) (smart-identity ctx v))
      v)))

(let [eval-string+ (memoize eval/safe-eval-string+)]
  (defn ^:export build-route' [ctx {:keys [:link/fiddle :link/tx-fn] :as link}]
    (if (and (not fiddle) tx-fn)
      (right nil)                                           ; :hf/remove doesn't have one by default, :hf/new does, both can be customized
      (mlet [fiddle-id (if fiddle
                         (right (:fiddle/ident fiddle))
                         (left {:message ":link/fiddle required" :data {:link link}}))
             f-wrap (if-let [formula-str (blank->nil (:link/formula link))]
                      (eval-string+ (str "(fn [ctx] \n" formula-str "\n)"))
                      (either/right (constantly (constantly nil))))
             f (try-either (f-wrap ctx))
             colored-args (try-either @(r/fmap (r/comp f (r/partial pull->colored-eid ctx)) (or (:hypercrud.browser/data ctx) (r/track identity nil))))
             :let [route (id->tempid (router/canonicalize fiddle-id (normalize-args colored-args)) ctx)]]
        (validated-route+ fiddle route ctx)))))

(def encode router/encode)
(def decode router/decode)

(defn decode' [route]
  (if route
    (try-either (decode route))
    (either/right nil)))
