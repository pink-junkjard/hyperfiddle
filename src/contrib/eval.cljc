(ns contrib.eval
  (:require [cats.monad.either :as either]
    #?@(:cljs [[cljs.analyzer :as analyzer]
               [cljs.js :as cljs]
               [cljs.tagged-literals :as tags]])
            [clojure.string :as string]
            [contrib.try :refer [try-either]]
            [taoensso.timbre :as timbre]

    ; This is contrib code, shouldn't be hyperfiddle deps
            [hyperfiddle.hc_data_readers :refer [hc-data-readers]]
            [hyperfiddle.readers :as hc-readers]))


(defn eval-string [code-str]
  {:pre [(string? code-str)
         (not (string/blank? code-str))]}
  ;; Hack - we don't understand why cljs compiler doesn't handle top level forms naturally
  ;; but wrapping in identity fixes the problem
  (let [code-str' (str "(identity\n" code-str "\n)")]
    #?(:clj  (try (either/right (load-string code-str'))
                  (catch Exception e
                    (timbre/debug code-str)
                    (timbre/error e)
                    (either/left e)))
       :cljs (binding [analyzer/*cljs-warning-handlers* []
                       tags/*cljs-data-readers* (merge tags/*cljs-data-readers*
                                                       hc-data-readers
                                                       {'entity hc-readers/entity
                                                        'uri hc-readers/uri})]
               (let [{value :value error :error :as eval-result} (cljs/eval-str (cljs/empty-state)
                                                                                code-str'
                                                                                nil
                                                                                {:eval cljs/js-eval}
                                                                                identity)]
                 (cond
                   error (either/left (ex-info "cljs eval failed" {:cljs-input code-str :cljs-result eval-result}))
                   :else (either/right value)))))))

(defn will-eval? [code-str]
  (or (:str (meta code-str))
      (and (string? code-str) (not (string/blank? code-str)))))

; todo cannot return stack traces inside a memoized fn
(def eval-str
  (memoize
    (fn [code-str]
      (cond
        (:str (meta code-str)) (either/right code-str)      ; if there is a string rep in the meta, the object itself is code

        (and (string? code-str) (not (string/blank? code-str))) (eval-string code-str)

        :else (either/right nil)))))

(defn eval-str-and-throw [code-str]
  (either/branch
    (eval-str code-str)
    (fn [e] (throw e))
    identity))

(defn -get-or-apply' "Apply a userland fn or return the val. The fn can crash. This should not exist, unify the apply with the eval and
use dynamic scope instead of args."
  [user-v & args]
  (if (fn? user-v)
    (try-either (apply user-v args))                        ; userland fn can crash
    (either/right user-v)))
