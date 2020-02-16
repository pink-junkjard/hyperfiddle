(ns contrib.eval-cljs
  (:require
    [cljs.analyzer :as ana]
    [cljs.js :as cljs]
    [cljs.env :as env]
    [shadow.cljs.bootstrap.browser :as boot]
    [cljs.tagged-literals :as tags]
    [hyperfiddle.hc_data_readers :refer [hc-data-readers]]
    [hyperfiddle.readers :as hc-readers]
    [promesa.core :as p]))


(defonce compile-state-ref (env/default-compiler-env))

(def booted ((p/promisify boot/init) compile-state-ref {:path "/static/dev/boot"}))

(defn eval-statement-str! [eval-in-ns code-str]
  {:pre [(string? code-str)]}
  (binding [ana/*cljs-warning-handlers* []
            tags/*cljs-data-readers* (merge tags/*cljs-data-readers*
                                            hc-data-readers
                                            {'entity hc-readers/entity
                                             'uri hc-readers/uri
                                             'long hc-readers/goog-math-long
                                             'schema hc-readers/schema})]
    (let [r (atom nil)]
      (when-not (contains? (::ana/namespaces @compile-state-ref) eval-in-ns)
        (cljs/eval-str compile-state-ref
                       (str "(ns " eval-in-ns ")")
                       nil
                       {:eval cljs/js-eval
                        :ns eval-in-ns
                        :context :statement}
                       (partial reset! r))
        (when-let [error (:error @r)]
          (throw error)))
      (cljs/eval-str compile-state-ref
                     code-str
                     nil
                     {:eval cljs/js-eval
                      :ns eval-in-ns
                      :context :statement}
                     (partial reset! r))
      (when-let [error (:error @r)]
        (throw error)))))

(defn eval-expr-str! [code-str]
  (binding [ana/*cljs-warning-handlers* []
            tags/*cljs-data-readers* (merge tags/*cljs-data-readers*
                                            hc-data-readers
                                            {'entity hc-readers/entity
                                             'uri hc-readers/uri
                                             'long hc-readers/goog-math-long
                                             'schema hc-readers/schema})]
    (let [r (atom nil)
          _ (cljs/eval-str compile-state-ref
                           code-str
                           nil
                           {:eval cljs/js-eval
                            :context :expr}
                           (partial reset! r))
          {value :value error :error :as eval-result} @r]
      (if error
        (throw (ex-info "cljs eval failed" {:cljs-input code-str :cljs-result eval-result}))
        value))))
