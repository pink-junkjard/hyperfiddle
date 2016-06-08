(ns hypercrud-client.util
  (:require [cljs.core.match :refer-macros [match]]
            [goog.Uri]
            [goog.string]
            [cognitect.transit :as t]))


;; transit uri encoder type
(deftype UriHandler []
  Object
  (tag [_ v] "r")
  (rep [_ v] (.toString v))
  (stringRep [this v] (.rep this v)))


;; allow goog.Uri as key in clojure map
(extend-type goog.Uri
  IHash
  (-hash [this]
    (goog.string/hashCode (pr-str this)))

  IEquiv
  (-equiv [this other]
    (and (instance? goog.Uri other)
         (= (hash this) (hash other)))))                    ;TODO find a better way to check equality


(def transit-encoding-opts {:handlers {goog.Uri (UriHandler.)}})
(def transit-decoding-opts {:handlers {"r" (fn [v] (goog.Uri. v))}})


(defn transit-decode
  "Transit decode an object from `s`."
  [s & {:keys [type opts]
        :or {type :json-verbose opts transit-decoding-opts}}]
  (let [rdr (t/reader type opts)]
    (t/read rdr s)))


(defn transit-encode
  "Transit encode `x` into a String."
  [x & {:keys [type opts]
        :or {type :json-verbose opts transit-encoding-opts}}]
  (let [wrtr (t/writer type opts)]
    (t/write wrtr x)))


(defn tempid? [eid] (< eid 0))


(defn entity->datoms [eid data]
  (->> data
       (mapcat (fn [[attr val]]
                 (cond
                   (coll? val) (map (fn [v] [:db/add eid attr v]) val)
                   :else [[:db/add eid attr val]])))
       (into [])))


(comment
  (def hc-data
    {:community/name "At Large in Ballard",
     :community/url "http://blog.seattlepi.com/ballard/",
     :community/neighborhood 17592186045456,
     :community/category #{"news" "human interest"},
     :community/orgtype 17592186045418,
     :community/type #{17592186045424}})

  (= (entity->datoms 1 hc-data)
     [[:db/add 1 :community/neighborhood 17592186045456]
      [:db/add 1 :community/type 17592186045424]
      [:db/add 1 :community/orgtype 17592186045418]
      [:db/add 1 :community/name "At Large in Ballard"]
      [:db/add 1 :community/category "news"]
      [:db/add 1 :community/category "human interest"]
      [:db/add 1 :community/url "http://blog.seattlepi.com/ballard/"]]))


(defn simplify [simplified-datoms next-datom]
  (let [[op e a v] next-datom
        g (group-by (fn [[op' e' a' v']] (and (= e' e) (= a' a) (= v' v)))
                    simplified-datoms)
        [op' e' a' v'] (first (get g true))                 ;if this count > 1, we have duplicate datoms, they are harmless and discard dups here.
        non-related (get g false)]
    (match [op]
           [:db/add] (if (= op' :db/retract)
                       non-related                          ;we have a related previous datom that cancels us and it out
                       (conj non-related next-datom))

           [:db/retract] (if (= op' :db/add)
                           non-related                      ;we have a related previous datom that cancels us and it out
                           (conj non-related next-datom)))))


(defn normalize-tx [old-datoms new-datoms]
  "We don't care about the cardinality (schema) because the UI code is always
  retracting values before adding new value, even in cardinality one case. This is a very
  convenient feature and makes the local datoms cancel out properly always to not cause
  us to re-assert datoms needlessly in datomic"
  (.log js/console (pr-str new-datoms))
  (reduce (fn [acc datom]
            ; each step, need to simplify against all prior datoms
            (simplify acc datom))
          old-datoms
          new-datoms))
