(ns hypercrud.compile.eval
  (:require [cats.monad.either :as either #?(:clj :refer :cljs :refer-macros) [try-either]]
    #?@(:cljs [[cljs.analyzer :as analyzer]
               [cljs.js :as cljs]
               [cljs.tagged-literals :as tags]])
            [hypercrud.compile.reader :refer [hc-data-readers]]
            [hypercrud.readers :as hc-readers]))


; todo cannot return stack traces inside a memoized fn
(def eval-str
  (memoize
    (fn [code-str]
      (cond
        (:str (meta code-str)) (either/right code-str)      ; if there is a string rep in the meta, the object itself is code

        (and (not (nil? code-str))
             (or (not (string? code-str))
                 (not (empty? code-str))))

        ;; Hack - we don't understand why cljs compiler doesn't handle top level forms naturally
        ;; but wrapping in identity fixes the problem
        (let [code-str' (str "(identity\n" code-str "\n)")]
          #?(:clj  (try-either (load-string code-str'))     ; todo clj has no notion of js/*
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
                         :else (either/right value))))))

        :else (either/left (ex-info "Cannot evaluate nil" {}))))))

(defn eval-str-and-throw [code-str]
  (either/branch
    (eval-str code-str)
    (fn [e] (throw e))
    identity))

(defn validate-user-code-str [code-str]
  (cond
    (:str (meta code-str)) code-str

    (and (not (nil? code-str))
         (or (not (string? code-str))
             (not (empty? code-str)))) code-str

    :else nil))
