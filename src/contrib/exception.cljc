(ns contrib.exception)                                      ; dead code ?


; a platform agnostic Error/Exception ctor
; todo unify this with hypercrud.types.Err/Err so raised exceptions are always serializable
(defn ->Exception [message]
  #?(:clj  (Exception. message)
     :cljs (js/Error message)))
