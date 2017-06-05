(ns hypercrud.types
  (:require [cljs.reader :as reader]))


(deftype DbId [id conn-id]
  Object (toString [_] (str "#DbId" (pr-str [id conn-id])))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o)))
  IComparable (-compare [x y] (compare (.-id x) (.-id y)))
  IHash (-hash [this] (hash [id conn-id]))
  IEquiv (-equiv [this other] (= (hash this) (hash other)))
  ILookup
  (-lookup [o k] (get o k nil))
  (-lookup [o k not-found] (case k
                             :id (.-id o)
                             :conn-id (.-conn-id o)
                             not-found)))

(def read-DbId #(apply ->DbId %))


(deftype DbVal [conn-id branch stage-hash #_history?]
  Object (toString [_] (str "#DbVal" (pr-str [conn-id branch stage-hash])))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o)))
  IHash (-hash [this] (hash [conn-id branch stage-hash]))
  IEquiv (-equiv [this other] (= (hash this) (hash other)))
  ILookup
  (-lookup [o k] (get o k nil))
  (-lookup [o k not-found] (case k
                             :conn-id (.-conn-id o)
                             :branch (.-branch o)
                             not-found)))

(def read-DbVal #(apply ->DbVal %))


(deftype DbError [msg]
  Object (toString [_] (str "#DbError" (pr-str msg)))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o))))

(def read-DbError #(->DbError %))


(reader/register-tag-parser! 'DbId read-DbId)
(reader/register-tag-parser! 'DbVal read-DbVal)
(reader/register-tag-parser! 'DbError read-DbError)

(deftype DbIdTransitHandler []
  Object
  (tag [_ v] "DbId")
  (rep [_ v] [(.-id v) (.-conn-id v)])
  (stringRep [_ v] nil)
  (getVerboseHandler [_] nil))


(defn DbIdTransitReader [v] (apply ->DbId v))


(deftype DbValTransitHandler []
  Object
  (tag [_ v] "DbVal")
  (rep [_ v] [(.-conn-id v) (.-branch v) (.-stage-hash v)])
  (stringRep [_ v] nil)
  (getVerboseHandler [_] nil))


(defn DbValTransitReader [v] (apply ->DbVal v))


(deftype DbErrorTransitHandler []
  Object
  (tag [_ v] "DbError")
  (rep [_ v] (.-msg v))
  (stringRep [_ v] nil)
  (getVerboseHandler [_] nil))

(defn DbErrorTransitReader [v] (->DbError v))


(deftype QueryRequest [query params pull-exps]
  Object (toString [_] (str "#QReq" (pr-str [query params pull-exps])))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o)))
  IHash (-hash [this] (hash [query params pull-exps]))
  IEquiv (-equiv [this other] (= (hash this) (hash other)))
  ILookup
  (-lookup [o k] (get o k nil))
  (-lookup [o k not-found] (case k
                             :query query
                             :params params
                             :pull-exps pull-exps
                             not-found)))

(deftype EntityRequest [e a db pull-exp]
  Object (toString [_] (str "#EReq" (pr-str [e a db pull-exp])))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o)))
  IHash (-hash [this] (hash [e a db pull-exp]))
  IEquiv (-equiv [this other] (= (hash this) (hash other)))
  ILookup
  (-lookup [o k] (get o k nil))
  (-lookup [o k not-found] (case k
                             :e e
                             :a a
                             :db db
                             :pull-exp pull-exp
                             not-found)))


(deftype QueryRequestTransitHandler []
  Object
  (tag [this v] "QReq")
  (rep [this v] [(.-query v) (.-params v) (.-pull-exps v)])
  (stringRep [this v] nil))

(deftype EntityRequestTransitHandler []
  Object
  (tag [this v] "EReq")
  (rep [this v] [(.-e v) (.-a v) (.-db v) (.-pull-exp v)])
  (stringRep [this v] nil))

(def read-QueryRequest #(apply ->QueryRequest %))           ; dedup
(def read-EntityRequest #(apply ->EntityRequest %))

(reader/register-tag-parser! 'QReq read-QueryRequest)
(reader/register-tag-parser! 'EReq read-EntityRequest)
