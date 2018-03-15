(ns hypercrud.browser.routing
  (:require [bidi.bidi :as bidi]
            [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [hypercrud.browser.dbname :as dbname]
            [hypercrud.compile.eval :as eval]
            [hypercrud.compile.reader :as reader]
            [hypercrud.types.Entity :refer [#?(:cljs Entity)]]
            [hypercrud.types.ThinEntity :refer [->ThinEntity #?(:cljs ThinEntity)]]
            [hypercrud.util.base-64-url-safe :as base64]
            [hypercrud.util.non-fatal :refer [try-either]]
            [hypercrud.util.core :refer [update-existing xorxs]]
            [hyperfiddle.runtime :as runtime])
  #?(:clj
     (:import (hypercrud.types.Entity Entity)
              (hypercrud.types.ThinEntity ThinEntity))))


(defn invert-route [domain route invert-id]
  (-> (walk/postwalk (fn [v]
                       (cond
                         (instance? Entity v) (assert false "hyperfiddle/hyperfiddle.net#150")

                         (instance? ThinEntity v)
                         (let [uri (get-in domain [:domain/environment (.-dbname v)])
                               id (invert-id (.-id v) uri)]
                           (->ThinEntity (.-dbname v) id))

                         :else v))
                     route)
      (update :fiddle-id (let [uri (:domain/fiddle-repo domain)]
                           #(invert-id % uri)))))

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
    (invert-route (:hypercrud.browser/domain ctx) route invert-id )))

(defn normalize-params [porps]
  {:pre [(not (:entity porps)) #_"legacy"
         ; There is some weird shit hitting this assert, like {:db/id nil}
         #_(not (map? porps)) #_"legacy"]}

  ; careful here -
  ; (seq [1]) - truthy
  ; (seq? [1]) - false
  ; (seq 1) - IllegalArgumentException
  (cond (instance? ThinEntity porps) [porps]                ; entity is also a map so do this first
        :else (xorxs porps)))

(defn ^:export build-route' [link ctx]
  (mlet [fiddle-id (if-let [page (:link/fiddle link)]
                     (either/right (or (:db/ident page) (:db/id page)))
                     (either/left {:message "link has no fiddle" :data {:link link}}))
         user-fn (eval/eval-str (:link/formula link))
         user-route-params (if user-fn
                             (try-either (user-fn ctx))
                             (either/right nil))
         :let [route-params (->> user-route-params
                                 (walk/postwalk (fn [v]
                                                  (if (instance? Entity v)
                                                    (let [dbname (some-> v .-uri (dbname/uri->dbname ctx))]
                                                      (->ThinEntity dbname (or (:db/ident v) (:db/id v))))
                                                    v)))
                                 (into {}))
               ;_ (timbre/debug route-params (-> (:link/formula link) meta :str))
               route-params (update-existing route-params :request-params normalize-params)]]
    (cats/return
      (id->tempid
        (merge route-params {:fiddle-id fiddle-id})
        ctx))))

(defn encode [route]
  ;/-/code-database    :fiddle-ident         ;fiddle-params       /query-params
  ;"/-/starter-blog-src2:schema!all-attributes;dbhole!name=$domains/"
  (str "/" (some-> route pr-str base64/encode)))

(defn decode [route-str]
  ; route-str must be well-formed (canonical redirect already happened)
  ; but is potentially invalid garbage from bots
  {:pre [(seq route-str)
         (string/starts-with? route-str "/")
         #_(not= "/" route-str)]
   :post [#_(do (println % route-str) true)
          #_(if % (:fiddle-id %))]}
  ; Urls in the wild get query params added because tweetdeck tools think its safe e.g.:
  ; http://localhost/hyperfiddle-blog/ezpkb21haW4gbmlsLCA6cHJvamVjdCAiaHlwZXJmaWRkbGUtYmxvZyIsIDpsaW5rLWRiaWQgI0RiSWRbMTc1OTIxODYwNDU4OTQgMTc1OTIxODYwNDU0MjJdLCA6cXVlcnktcGFyYW1zIHs6ZW50aXR5ICNEYklkWzE3NTkyMTg2MDQ2MTMyIDE3NTkyMTg2MDQ1ODgyXX19?utm_content=buffer9a24a&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer
  (let [[_ route-encoded-and-query-params] (string/split route-str #"/")]
    (cond
      (not (nil? route-encoded-and-query-params))
      (let [[route-encoded url-param-garbage] (string/split route-encoded-and-query-params #"\?")]
        (reader/read-string (base64/decode route-encoded)))

      ; no route, or garbage from http crawlers
      :else nil)))

(defn decode' [route]
  (if route
    (try-either (decode route))
    (either/right nil)))


; Bidi routing protocols
(def regex-keyword "^:[A-Za-z]+[A-Za-z0-9\\*\\+\\!\\-\\_\\?\\.]*(?:%2F[A-Za-z]+[A-Za-z0-9\\*\\+\\!\\-\\_\\?\\.]*)?")
(def regex-string "^[A-Za-z0-9\\-\\_\\.]+")
(def regex-long "^[0-9]+")

(extend-type ThinEntity

  ; must be safe to user input. if they throw, the app won't fallback to a system-route

  bidi/PatternSegment
  (segment-regex-group [_]
    ; the value mathign the placeholder can be a keyword or a long
    (str "(?:" regex-long ")|(?:" regex-keyword ")"))
  (param-key [this]
    ; Abusing .-id to store the param-key in the entity placeholder in the route
    (.-id this))
  (transform-param [this]
    (fn [v]
      (let [$ (.-dbname this)                               ; the "$" is provided by entity placeholder in the route
            e (reader/read-edn-string v)]                   ; the reader will need to subs ! to /
        (->ThinEntity $ e))))
  (matches? [this s]
    (let [r (re-pattern
              "[A-Za-z]+[A-Za-z0-9\\*\\+\\!\\-\\_\\?\\.]*(?:%2F[A-Za-z]+[A-Za-z0-9\\*\\+\\!\\-\\_\\?\\.]*)?")]
      (re-matches r s)))
  (unmatch-segment [this params]
    (let [entity (get params (.-id this))]
      ; lookup refs not implemented, but eid and ident work
      ; safe
      (some-> entity .-id)))

  bidi/Pattern
  (match-pattern [this env]
    ; is this even in play? I don't think I ever hit this bp
    (let [read (reader/read-edn-string (:remainder env))]
      (-> env
          (update-in [:route-params] assoc (.-id this) (->ThinEntity (.-dbname this) read))
          ; totally not legit to count read bc whitespace
          (assoc :remainder (subs (:remainder env) 0 (count (pr-str read)))))))
  (unmatch-pattern [this m]
    (let [param-key (.-id this)]
      (-> m :params (get param-key) .-id pr-str)))

  ;bidi/Matched
  ;(resolve-handler [this m] (bidi/succeed this m))
  ;(unresolve-handler [this m] (when (= this (:handler m)) ""))
  )


; Bidi is not a great fit for the sys router because there is only one route with varargs params
; params are dynamically typed; need to model an `any`, eager-consume the path segment,
; and figure out the edn from the path segment. How to determine #entity vs scalar?
;(def sys-router
;  [["/" :fiddle-id "/" #edn 0] :browse])