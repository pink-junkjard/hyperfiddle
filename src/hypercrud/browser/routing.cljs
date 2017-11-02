(ns hypercrud.browser.routing
  (:require [cljs.reader :as reader]
            [clojure.set :as set]
            [clojure.string :as string]
            [hypercrud.browser.context-util :as context-util]
            [hypercrud.types.Entity :refer [->Entity Entity]]
            [hypercrud.types.ThinEntity :refer [->ThinEntity ThinEntity]]
            [hypercrud.util.base-64-url-safe :as base64]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util]
            [reagent.core :as reagent]))


(defn invert-ids [route invert-id ctx]
  (-> route
      (update :link-id (let [uri (context-util/code-ident->database-uri (:code-database route) ctx)]
                         #(invert-id % uri)))
      (update :query-params
              (partial util/map-values
                       (fn [v]
                         ; todo support other types of v (map, vector, etc)
                         (cond
                           (instance? Entity v)
                           (let [id (invert-id (:db/id v) (-> v .-dbval .-uri))]
                             (->Entity (.-dbval v) (assoc (.-coll v) :db/id id)))

                           (instance? ThinEntity v)
                           (let [uri (context-util/ident->database-uri (.-dbname v) ctx)
                                 id (invert-id (.-id v) uri)]
                             (->ThinEntity (.-dbname v) id))

                           :else v))))))

(defn ctx->id-lookup [uri ctx]
  ; todo tempid-lookups need to be indexed by db-ident not val
  (let [stage-val @(reagent/cursor (.-state-atom (:peer ctx)) [:stage])
        branch-val (branch/branch-val uri (:branch ctx) stage-val)]
    ; todo what about if the tempid is on a higher branch in the uri?
    @(reagent/cursor (.-state-atom (:peer ctx)) [:tempid-lookups uri branch-val])))

(defn id->tempid [route ctx]
  (let [invert-id (fn [id uri]
                    (let [id->tempid (ctx->id-lookup uri ctx)]
                      (get id->tempid id id)))]
    (invert-ids route invert-id ctx)))

(defn tempid->id [route ctx]
  (let [invert-id (fn [temp-id uri]
                    (let [tempid->id (-> (ctx->id-lookup uri ctx)
                                         (set/map-invert))]
                      (get tempid->id temp-id temp-id)))]
    (invert-ids route invert-id ctx)))

(defn encode
  ([route]
   (str "/" (some-> route pr-str base64/encode)))
  ([route external-hostname]
   (str (some->> external-hostname (str "http://"))
        (encode route))))

(defn decode [route-str]
  (assert (string/starts-with? route-str "/"))

  ; Urls in the wild get query params added because tweetdeck tools think its safe e.g.:
  ; http://localhost/hyperfiddle-blog/ezpkb21haW4gbmlsLCA6cHJvamVjdCAiaHlwZXJmaWRkbGUtYmxvZyIsIDpsaW5rLWRiaWQgI0RiSWRbMTc1OTIxODYwNDU4OTQgMTc1OTIxODYwNDU0MjJdLCA6cXVlcnktcGFyYW1zIHs6ZW50aXR5ICNEYklkWzE3NTkyMTg2MDQ2MTMyIDE3NTkyMTg2MDQ1ODgyXX19?utm_content=buffer9a24a&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer
  (let [[_ route-encoded-and-query-params] (string/split route-str #"/")]
    (cond
      (not (nil? route-encoded-and-query-params))
      (let [[route-encoded url-param-garbage] (string/split route-encoded-and-query-params #"\?")]
        (reader/read-string (base64/decode route-encoded)))

      ; The only way to get to / is if the user types it in. We never ever link to /, and nginx & node should redirect to the canonical.
      :else nil)))
