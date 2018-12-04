(ns contrib.eval-cljs
  (:require-macros
    [contrib.eval-cljs :refer [build-cljsjs-empty-state]])
  (:require
    [cljs.analyzer :as ana]
    [cljs.js :as cljs]
    [cljs.tagged-literals :as tags]
    [cognitect.transit]
    [hyperfiddle.hc_data_readers :refer [hc-data-readers]]
    [hyperfiddle.readers :as hc-readers]))


(def ^:private -cljsjs-empty-state (build-cljsjs-empty-state))

(defn eval-statement-str! [eval-in-ns code-str]
  {:pre [(string? code-str)]}
  (binding [ana/*cljs-warning-handlers* []
            tags/*cljs-data-readers* (merge tags/*cljs-data-readers*
                                            hc-data-readers
                                            {'entity hc-readers/entity
                                             'uri hc-readers/uri})]
    (let [r (atom nil)]
      (when-not (contains? (::ana/namespaces @-cljsjs-empty-state) eval-in-ns)
        (cljs/eval-str -cljsjs-empty-state
                       (str "(ns " eval-in-ns ")")
                       nil
                       {:eval cljs/js-eval
                        :ns eval-in-ns
                        :context :statement}
                       (partial reset! r))
        (when-let [error (:error @r)]
          (throw error)))
      (cljs/eval-str -cljsjs-empty-state
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
                                             'uri hc-readers/uri})]
    (let [r (atom nil)
          _ (cljs/eval-str -cljsjs-empty-state
                           code-str
                           nil
                           {:eval cljs/js-eval
                            :context :expr}
                           (partial reset! r))
          {value :value error :error :as eval-result} @r]
      (if error
        (throw (ex-info "cljs eval failed" {:cljs-input code-str :cljs-result eval-result}))
        value))))
