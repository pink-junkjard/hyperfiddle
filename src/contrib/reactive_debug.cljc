(ns contrib.reactive-debug
  (:require [taoensso.timbre :as timbre]))


#?(:cljs
   (defn track-cmp [c-name c & args]
     (timbre/info (str "tracker[" c-name "]:mount"))
     (let [state (atom nil)
           f (fn x [path v & [initial]]
               (cond
                 (map? v)
                 (doseq [[k v] v]
                   (x (conj path k) v initial))

                 (or (seq? v) (vector? v))
                 (doseq [[i v] (map-indexed (fn [i v] [i v]) v)]
                   (x (conj path i) v initial))

                 :else (swap! state update-in path
                              (fn [o]
                                (when (or (not initial) (not= o v))
                                  (timbre/info (str (pr-str path) ":\n\told:" (pr-str o) "\n\tnew:" (pr-str v))))
                                v))))]
       (f [] args true)
       (fn [c-name c & args]
         (timbre/info (str "tracker[" c-name "]:render"))
         (f [] args)
         ; return comp
         (into [c] args)))))

(defmacro track [c & args]
  (into ['contrib.reactive-debug/track-cmp (str c) c] args))
