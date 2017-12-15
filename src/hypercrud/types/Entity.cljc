(ns hypercrud.types.Entity
  #?(:clj
     (:import [clojure.lang Counted IFn IHashEq ILookup Seqable]
              [clojure.core.protocols IKVReduce])))


(defn- impl-hash [o]
  (hash [(.-dbval o) (:db/id (.-coll o))]))

(defn- impl-print [o]
  (pr-str (.-coll o)))

(deftype Entity [dbval coll]
  #?@(:clj  [Object
             (equals [o other]
               (and (instance? Entity other)
                    (= (.-dbval o) (.-dbval other))
                    (= (:db/id (.-coll o)) (:db/id (.-coll other)))))

             IHashEq
             (hasheq [o] (impl-hash o))

             Seqable
             (seq [o] (seq coll))

             Counted
             (count [o] (count coll))

             ILookup
             (valAt [o k] (get coll k))
             (valAt [o k not-found] (get coll k not-found))

             IKVReduce
             (kv_reduce [o f init] (reduce-kv coll f init))

             IFn
             (invoke [o k] (coll k))
             (invoke [o k not-found] (coll k not-found))]

      :cljs [Object
             (toString [o] (impl-print o))

             IPrintWithWriter
             (-pr-writer [o writer _] (-write writer (.toString o)))

             IEquiv
             (-equiv [o other]
                     (and (instance? Entity other)
                          (= (.-dbval o) (.-dbval other))
                          (= (:db/id (.-coll o)) (:db/id (.-coll other)))))

             IHash
             (-hash [o] (impl-hash o))

             IIterable
             (-iterator [o] (-iterator coll))

             ISeqable
             (-seq [o] (seq coll))

             ICounted
             (-count [o] (count coll))

             ILookup
             (-lookup [o k] (get coll k))
             (-lookup [o k not-found] (get coll k not-found))

             IFind
             (-find [o k] (-find coll k))

             IKVReduce
             (-kv-reduce [o f init] (reduce-kv coll f init))

             IFn
             (-invoke [o k] (coll k))
             (-invoke [o k not-found] (coll k not-found))]))

#?(:clj
   (defmethod print-method Entity [o ^java.io.Writer w]
     (.write w (impl-print o))))

#?(:clj
   (defmethod print-dup Entity [o w]
     (print-method o w)))
