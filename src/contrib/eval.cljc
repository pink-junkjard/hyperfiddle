(ns contrib.eval
  (:require
    #?@(:cljs [[cljs.analyzer :as analyzer]
               [cljs.js :as cljs]
               [cljs.tagged-literals :as tags]])
    [clojure.string :as string]
    ; This is contrib code, shouldn't be hyperfiddle deps
    [hyperfiddle.hc_data_readers :refer [hc-data-readers]]
    [hyperfiddle.readers :as hc-readers]))


(defn eval-string [code-str]
  {:pre [(string? code-str)
         (not (string/blank? code-str))]}
  ;; Hack - we don't understand why cljs compiler doesn't handle top level forms naturally
  ;; but wrapping in identity fixes the problem
  (let [code-str' (str "(identity\n" code-str "\n)")]
    #?(:clj  (load-string code-str')
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
                   error (throw (ex-info "cljs eval failed" {:cljs-input code-str :cljs-result eval-result}))
                   :else value))))))
