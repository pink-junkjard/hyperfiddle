(ns contrib.cljs-platform
  #?(:cljs (:require-macros [contrib.cljs-platform])))


; https://cljs.github.io/api/cljs.core/STARtargetSTAR

(defn nodejs? []
  #?(:cljs (= *target* "nodejs")
     :clj  false #_"compiler"))

(defn browser? []
  #?(:cljs (= *target* "default")
     :clj  false #_"compiler"))

;Loading src/contrib/cljs_platform.cljc...
;CompilerException java.lang.ClassNotFoundException: cljs.env, compiling:(/Users/dustin/src/hfnet/hyperfiddle/src/contrib/cljs_platform.cljc:16:14)
(defn- nodejs-target? "only works in macros" []
  (= :nodejs (get-in @cljs.env/*compiler* [:options :target])))

; a compile time check for environment
; most of the time a appfn check will do (= "nodejs" *target*)
; which is also public runtime, unlike these macros
; https://stackoverflow.com/a/47499855
(defmacro code-for-nodejs
  [& body]
  (when (nodejs-target?)
    `(do ~@body)))

(defmacro code-for-browser
  [& body]
  (when-not (nodejs-target?)
    `(do ~@body)))

#?(:cljs                                                    ; runtime only
   (defn global! []
     (cond (nodejs?) js/global
           (browser?) js/window)))

#?(:cljs                                                    ; runtime only
   (defn merge-user! [m]
     (let [g (global!)]
       (if-not (aget g "user") (aset g "user" #js {}))
       (doseq [[k v] m]
         ; They are referenced using FFI syntax: `js/user.docs-fiddle-ident-link`
         (aset js/user k v)))))
