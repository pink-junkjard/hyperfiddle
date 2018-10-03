(ns contrib.pprint
  #?(:cljs (:require-macros [contrib.pprint]))
  (:require
    [clojure.pprint]
    [clojure.string :as string]
    [hyperfiddle.pprint]))


(defn ^:export pprint [o]
  (binding [clojure.pprint/*print-pprint-dispatch* hyperfiddle.pprint/simple-dispatch]
    (clojure.pprint/pprint o)))

(defn ^:export pprint-str [v & [columns]]
  (string/trimr
    ; Previously, clojure.pprint/*print-miser-width* was set to nil in main
    (binding [clojure.pprint/*print-right-margin* (or columns clojure.pprint/*print-right-margin*)]
      (with-out-str (pprint v)))))

(defn pprint-datoms-str [vv]
  (if (nil? vv)
    ""
    (let [vs (->> (map pr-str vv)
                  (interpose "\n ")
                  vec)]
      (->> (cons "[" (conj vs "]"))
           (apply str)))))
