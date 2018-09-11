(ns contrib.pprint
  #?(:cljs (:require-macros [contrib.pprint]))
  (:require
    [clojure.pprint]
    [clojure.string :as string]
    [contrib.string :refer [blank->nil]]
    [hyperfiddle.pprint]
    [net.cgrand.packed-printer :as packed-printer]))


(defn ^:export slow-pprint-str [v & [columns]]
  (with-out-str
    (packed-printer/pprint v :width (or columns clojure.pprint/*print-right-margin*))))

(defn ^:export pprint [o]
  (binding [clojure.pprint/*print-pprint-dispatch* hyperfiddle.pprint/simple-dispatch]
    (clojure.pprint/pprint o)))

(defn ^:export pprint-str [v & [columns]]
  (clojure.string/trimr
    ; Previously, clojure.pprint/*print-miser-width* was set to nil in main
    (binding [clojure.pprint/*print-right-margin* (or columns clojure.pprint/*print-right-margin*)]
      (with-out-str (pprint v)))))

#?(:clj (defmacro mpprint-str [& args] (apply slow-pprint-str args)))

(defn pprint-datoms-str [vv]
  (if (nil? vv)
    ""
    (let [vs (->> (map pr-str vv)
                  (interpose "\n ")
                  vec)]
      (->> (cons "[" (conj vs "]"))
           (apply str)))))
