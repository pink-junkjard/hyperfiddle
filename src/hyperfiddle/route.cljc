(ns hyperfiddle.route
  (:require
    [cats.core :refer [>>=]]
    [cats.monad.either :as either]
    [clojure.spec.alpha :as s]
    [clojure.string :as string]
    [clojure.walk]
    [contrib.data]
    [contrib.ednish :as ednish :refer [decode-ednish encode-ednish]]
    [contrib.pprint :refer [pprint-str]]
    [contrib.rfc3986 :refer [decode-rfc3986-pchar encode-rfc3986-pchar]]
    [contrib.string :refer [empty->nil]]
    [contrib.try$ :refer [try-either]]
    [cuerdas.core :as str]
    [hypercrud.types.ThinEntity :refer [->ThinEntity #?(:cljs ThinEntity)]]
    [hyperfiddle.fiddle]                                    ; for ::fiddle spec
    [taoensso.timbre :as timbre])
  #?(:clj
     (:import (hypercrud.types.ThinEntity ThinEntity))))


(s/def ::fiddle (s/or
                  :ident :fiddle/ident
                  :dbid number?
                  ;:lookup ...
                  :uuid :fiddle/uuid))
(s/def ::datomic-args vector?)
(s/def ::service-args some?)
(s/def ::fragment string?)

(s/def :hyperfiddle/route
  (s/keys
    :req [::fiddle]
    :opt [::datomic-args ::service-args ::fragment]))

(defn validate-route+ [route]
  (if (s/valid? :hyperfiddle/route route)
    (either/right route)
    (either/left (ex-info (str "Invalid route\n" (s/explain-str :hyperfiddle/route route))
                          (s/explain-data :hyperfiddle/route route)))))

(defn equal-without-frag? [a b]
  (= (dissoc a ::fragment) (dissoc b ::fragment)))

(defn decoding-error [e s]
  {::fiddle :hyperfiddle.system/decoding-error
   ::datomic-args [s (ex-message e) (pprint-str (ex-data e))]})

(defn url-encode [route home-route]
  {:pre [(s/valid? :hyperfiddle/route route) (s/valid? :hyperfiddle/route home-route)]
   :post [(str/starts-with? % "/")]}
  (let [{:keys [::fiddle ::datomic-args ::service-args ::fragment]} route]
    (if (equal-without-frag? route home-route)
      (str "/" (some->> fragment empty->nil (str "#")))
      (case fiddle
        :hyperfiddle.system/decoding-error (first datomic-args)
        (str "/"
             (ednish/encode-uri fiddle)
             "/"
             (str/join "/" (map ednish/encode-uri datomic-args))
             ; hash and query aren't used today, todo i would prefer to encode as edn hashmap instead of k=v
             (if (seq service-args) (str "?" (str/join "&" (->> service-args (map (fn [[k v]] (ednish/encode-uri k "=" (ednish/encode-uri v))))))))
             (if (empty->nil fragment) (str "#" (-> fragment encode-ednish encode-rfc3986-pchar))))))))


(def url-regex #"/([^/?#]*)(?:/([^?#]*))?(?:\?([^#]*))?(?:#(.*))?")
;                /|________|  /|______|      ?|_____|     #|__|
;                     |           |              |          |
;      url        seg[0]path   seg[1 ...]     query        fragment
;      hf-route   fiddle       datomic-args   service-args fragment

(defn url-decode [s home-route]
  {:pre [(str/starts-with? s "/") (s/valid? :hyperfiddle/route home-route)]
   :post [(s/valid? :hyperfiddle/route %)]}
  (let [[path s-fragment] (string/split s #"#" 2)
        fragment (-> s-fragment decode-rfc3986-pchar decode-ednish empty->nil)]
    (if (= "/" path)
      (cond-> home-route
        fragment (assoc ::fragment fragment))
      (-> (try-either
            (let [[_ s-fiddle s-datomic-args s-service-args s-fragment] (re-find url-regex s)]
              (contrib.data/dissoc-nils
                {::fiddle (ednish/decode-uri s-fiddle)
                 ::datomic-args (some-> (->> (str/split s-datomic-args "/") ; careful: (str/split "" "/") => [""]
                                             (remove str/empty-or-nil?)
                                             (map ednish/decode-uri)
                                             seq)
                                        vec)
                 ::service-args (ednish/decode-uri s-service-args)
                 ::fragment (-> s-fragment decode-rfc3986-pchar decode-ednish empty->nil)})))
          (>>= validate-route+)
          (either/branch
            (fn [e] (decoding-error e s))
            identity)))))

(defn invert-datomic-arg [v invert-id]
  (if (instance? ThinEntity v)
    (->ThinEntity (.-dbname v) (invert-id (.-dbname v) (.-id v)))
    v))

(defn invert-datomic-args [invert-id datomic-args]
  (mapv #(invert-datomic-arg % invert-id) datomic-args))

(defn invert-route [route invert-id]
  (if (contains? route ::datomic-args)
    (update route ::datomic-args (partial invert-datomic-args invert-id))
    route))

(defn legacy-route-adapter [route]
  (cond
    (nil? route) route
    (map? route) route
    (vector? route) (let [[fiddle datomic-args service-args fragment] route
                          m-route (contrib.data/dissoc-nils
                                    {::fiddle fiddle
                                     ::datomic-args datomic-args
                                     ::service-args service-args
                                     ::fragment fragment})]
                      (timbre/warnf "Deprecated route format detected `%s` use a map instead: `%s`" (pr-str route) (pr-str m-route))
                      m-route)))
