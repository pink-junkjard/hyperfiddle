(ns contrib.eval
  (:require
    #?@(:cljs [[cljs.analyzer :as analyzer]
               [cljs.js :as cljs]
               [cljs.tagged-literals :as tags]])
    [clojure.string :as string]
    [cats.monad.either :as either :refer [left right]]
    [contrib.try$ :refer [try-either]]
    ; This is contrib code, shouldn't be hyperfiddle deps
    [hyperfiddle.hc_data_readers :refer [hc-data-readers]]
    [hyperfiddle.readers :as hc-readers]))


#?(:cljs (def ^:private -cljs-empty-state-val @(cljs/empty-state)))

(defn eval-string! [code-str]
  {:pre [(string? code-str) (not (string/blank? code-str))]}
  ;; Hack - we don't understand why cljs compiler doesn't handle top level forms naturally
  ;; but wrapping in identity fixes the problem
  (let [code-str' (str "(identity\n" code-str "\n)")]
    #?(:clj  (load-string code-str')
       :cljs (binding [analyzer/*cljs-warning-handlers* []
                       tags/*cljs-data-readers* (merge tags/*cljs-data-readers*
                                                       hc-data-readers
                                                       {'entity hc-readers/entity
                                                        'uri hc-readers/uri})]
               (let [{value :value error :error :as eval-result} (cljs/eval-str (atom -cljs-empty-state-val)
                                                                                code-str'
                                                                                nil
                                                                                {:eval cljs/js-eval}
                                                                                identity)]
                 (cond
                   error (throw (ex-info "cljs eval failed" {:cljs-input code-str :cljs-result eval-result}))
                   :else value))))))

(defn safe-eval-string+ [code-str]
  (try-either (eval-string! code-str)))

(let [safe-eval-string #(try-either (when % (eval-string! %)))
      memoized-eval-string (memoize safe-eval-string)]
  (defn ensure-fn [s]
    (if-not (string? s)
      s
      (either/branch
        (memoized-eval-string s)
        #(constantly (pr-str %))                            ; print the compile error when invoked
        (fn [user-fn]                                       ; eventually invoke this unsafe fn
          #(either/branch
             (try-either (user-fn %))
             str                                            ; print the runtime error when invoked
             identity))))))
