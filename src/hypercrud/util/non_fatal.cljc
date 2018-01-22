(ns hypercrud.util.non-fatal
  #?(:cljs (:require-macros [hypercrud.util.non-fatal :refer [try-catch-non-fatal try-either]]))
  (:require [cats.monad.either :as either]))


#?(:clj
   (defmacro try-catch-non-fatal [& args]
     (let [[try-body [e catch-body]] (split-at (- (count args) 2) args)]
       `(try
          ~@try-body
          ~(if (:ns &env)
             `(catch js/Error ~e ~catch-body)
             `(catch Throwable ~e
                (if (#{VirtualMachineError
                       ThreadDeath
                       InterruptedException
                       LinkageError} (type ~e))
                  (throw ~e)
                  ~catch-body)))))))

#?(:clj
   (defmacro try-either
     "Try to evalute the body and return the result as an either.
     If a non-fatal throwable is thrown return the exception as a left,
     otherwise return the result as a right."
     [& body]
     (let [e (gensym)]
       `(try-catch-non-fatal (either/right (do ~@body)) ~e (either/left ~e)))))
