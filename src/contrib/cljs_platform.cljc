(ns contrib.cljs-platform
  #?(:cljs (:require-macros [contrib.cljs-platform])))


; https://cljs.github.io/api/cljs.core/STARtargetSTAR

(defn nodejs? []
  #?(:cljs (= *target* "nodejs")
     :clj  false #_"compiler"))

(defn browser? []
  #?(:cljs (= *target* "default")
     :clj  false #_"compiler"))

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

#?(:cljs
   (defn global! []
     (cond (nodejs?) js/global
           (browser?) js/window)))

(defn install-on-global! [m]
  (let [g (global!)]
    (doseq [[k v] m]
      (aset g k v))))
