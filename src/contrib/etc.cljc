(ns contrib.etc)

(defn first-key [k]
  (cond (ident? k) k
        (seqable? k) (first k)
        :or (throw (IllegalArgumentException.))))

(defn to-keys [v]
  (cond (seqable? v) v
        :or [v]))

(defn tag [xs]
  (keyword (first-key xs)))

(defn in-ns? [ns x]
  (= (str ns) (namespace (keyword x))))

(defn with-ns [n k]
  (keyword (name n) (name k)))

(defn trim-str [s]
  (if-let [indent (some-> (re-find #"(\n +)\S" s) second)]
    (clojure.string/trim (clojure.string/replace s indent "\n"))
    (clojure.string/trim s)))
