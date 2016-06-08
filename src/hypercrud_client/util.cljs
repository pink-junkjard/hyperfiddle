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


(defn normalized-datoms->entities [schema datoms]
  (let [indexed-schema (group-by :db/ident schema)]
    (->> datoms
         (reduce (fn [acc [op e a v]]
                   (match [op (get-in indexed-schema [a 0 :db/cardinality])]
                          [:db/add :db.cardinality/one] (update-in acc [e] assoc a v)
                          [:db/retract :db.cardinality/one] (update-in acc [e] dissoc a)
                          [:db/add :db.cardinality/many] (update-in acc [e a] (fn [oldv] (if oldv (conj oldv v) #{v})))
                          [:db/retract :db.cardinality/many] (update-in acc [e a] (fn [oldv] (if oldv (disj oldv v) #{})))))
                 {})
         (map (fn [[eid entity]] (assoc entity :db/id eid)))
         (into #{}))))


(defn simplify [indexed-schema prior-datoms next-datom]
  (let [[op e a v] next-datom
        cardinality (get-in indexed-schema [a 0 :db/cardinality])]
    (reduce
      (fn [acc [op' e' a' v' :as prior-datom]]
        (match [op cardinality]

               ; cardinality/one: :add overrides prior :add, :retract overrides prior :retract
               [_ :db.cardinality/one] (if (and (= e' e) (= a' a))
                                         (conj acc next-datom) ; override - reject old one, accept new one
                                         (conj acc prior-datom next-datom)) ; no conflict - accept both

               ; :add cancels prior :retract of matching value
               [:db/add :db.cardinality/many] (if (and (= op' :db/retract) (= e' e) (= a' a) (= v' v))
                                                acc         ; cancel them out - no net change
                                                (conj acc prior-datom next-datom)) ; no conflict - accept both

               ; :retract cancels prior :add of matching value
               [:db/retract :db.cardinality/many] (if (and (= op' :db/add) (= e' e) (= a' a) (= v' v))
                                                    acc     ; cancel them out, no net change
                                                    (conj acc prior-datom next-datom)))) ; no conflict - accept both
      []
      prior-datoms)))


(defn simplify2 [indexed-schema simplified-datoms next-datom]
  (let [[op e a v] next-datom
        cardinality (get-in indexed-schema [a 0 :db/cardinality])
        predicate (match [op cardinality]
                         ; cardinality/one: :add overrides prior :add, :retract overrides prior :retract
                         [_ :db.cardinality/one] (fn [[op' e' a' v']] (and (= e' e) (= a' a)))

                         ; :add cancels prior :retract of matching value
                         [:db/add :db.cardinality/many] (fn [[op' e' a' v']] (and (= e' e) (= a' a) (= v' v)))

                         ; :retract cancels prior :add of matching value
                         [:db/retract :db.cardinality/many] (fn [[op' e' a' v']] (and (= op' :db/add) (= e' e) (= a' a) (= v' v))))
        g (group-by predicate simplified-datoms)
        [related-datom] (get g true)
        non-related (get g false)]
    (match [op cardinality]
           ; cardinality/one: :add overrides prior :add, :retract overrides prior :retract
           [_ :db.cardinality/one] (conj non-related next-datom)

           ; :add cancels prior :retract of matching value
           [:db/add :db.cardinality/many] (if (and related-datom (= (first related-datom) :db/retract)) ;(first related-datom) = op
                                            non-related     ;we have a related previous datom that cancels us and it out)
                                            (conj non-related next-datom))

           ; :retract cancels prior :add of matching value
           [:db/retract :db.cardinality/many] (if (and related-datom (= (first related-datom) :db/add)) ;(first related-datom) = op
                                                non-related ;we have a related previous datom that cancels us and it out)
                                                (conj non-related next-datom)))
    (conj non-related next-datom)))



(defn normalize-tx [schema datoms]
  (let [indexed-schema (group-by :db/ident schema)]
    (reduce (fn [acc datom]
              ; each step, need to simplify against all prior datoms
              (simplify2 indexed-schema acc datom))
            []
            datoms)))
