(ns hyperfiddle.pprint
  (:require
    [clojure.pprint]
    [hypercrud.types.ThinEntity :refer [#?(:cljs ThinEntity)]]
    #?(:cljs [reagent.ratom]))
  #?(:clj
     (:import
       hypercrud.types.ThinEntity.ThinEntity)))


(defn pprint-simple-thinentity [o]
  (clojure.pprint/pprint-logical-block
    :prefix "#entity[" :suffix "]"
    (clojure.pprint/write-out (.-dbname o))
    (#?(:clj .write :cljs -write) *out* " ")
    (clojure.pprint/pprint-newline :linear)
    (clojure.pprint/write-out (.-id o))))

#?(:cljs
   (defn pprint-ratom [o s]
     (clojure.pprint/pprint-logical-block
       :prefix (str "#<" s " ") :suffix ">"
       (binding [#?@(:cljs [reagent.ratom/*ratom-context* nil])]
         (clojure.pprint/write-out (deref o))))))

(comment
  ; cljs's simple-dispatch is not extensible, otherwise this is what we want:
  (defmethod clojure.pprint/simple-dispatch ThinEntity [o] (pprint-simple-thinentity o)))

(defn simple-dispatch [o]
  (condp = (type o)
    #?@(:cljs
        [reagent.ratom/RAtom (pprint-ratom o "Atom:")
         reagent.ratom/Track (pprint-ratom o "Track:")
         reagent.ratom/RCursor (pprint-ratom o "Cursor:")
         reagent.ratom/Reaction (pprint-ratom o (str "Reaction " (hash o) ":"))
         reagent.ratom/Wrapper (pprint-ratom o "Wrap:")])
    ThinEntity (pprint-simple-thinentity o)
    (clojure.pprint/simple-dispatch o)))

(comment
  (binding [clojure.pprint/*print-pprint-dispatch* hyperfiddle.pprint/simple-dispatch]
    (clojure.pprint/pprint '...)))
