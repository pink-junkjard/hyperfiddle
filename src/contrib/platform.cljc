(ns contrib.platform
  #?(:cljs (:require-macros [contrib.platform])))


; https://github.com/plumatic/schema/blob/56bb34ec95045e4a3078dd8237a9f4d3196040ec/src/clj/schema/macros.clj#L13-20
; https://groups.google.com/forum/#!topic/clojurescript/iBY5HaQda4A
(defn cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))

#?(:clj
   (defmacro if-cljs
     "Return then if we are generating cljs code and else for Clojure code.
      https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"
     [then else]
     (if (cljs-env? &env) then else)))

; Macro to dynaload datomic at JVM runtime but not in cljs macros
#?(:clj
   (defmacro code-for-jvm [body]
     (if-not (cljs-env? &env)
       body)))
