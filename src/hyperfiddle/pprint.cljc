(ns hyperfiddle.pprint
  (:require
    [clojure.pprint]
    [hypercrud.types.Entity :refer [#?(:cljs Entity)]]
    [hypercrud.types.ThinEntity :refer [#?(:cljs ThinEntity)]])
  #?(:clj
     (:import
       hypercrud.types.Entity.Entity
       hypercrud.types.ThinEntity.ThinEntity)))


(defn pprint-simple-entity [o] (clojure.pprint/write-out (.-coll o)))

(defn pprint-simple-thinentity [o]
  (clojure.pprint/pprint-logical-block
    :prefix "#entity[" :suffix "]"
    (clojure.pprint/write-out (.-dbname o))
    (.write *out* " ")
    (clojure.pprint/pprint-newline :linear)
    (clojure.pprint/write-out (.-id o))))

(comment
  ; cljs's simple-dispatch is not extensible, otherwise this is what we want:
  (defmethod clojure.pprint/simple-dispatch Entity [o] (pprint-simple-entity o))
  (defmethod clojure.pprint/simple-dispatch ThinEntity [o] (pprint-simple-thinentity o)))

(defn simple-dispatch [o]
  (condp = (type o)
    Entity (pprint-simple-entity o)
    ThinEntity (pprint-simple-thinentity o)
    (clojure.pprint/simple-dispatch o)))

(comment
  (binding [clojure.pprint/*print-pprint-dispatch* hyperfiddle.pprint/simple-dispatch]
    (clojure.pprint/pprint '...)))
