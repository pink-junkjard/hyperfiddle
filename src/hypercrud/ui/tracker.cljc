(ns hypercrud.ui.tracker
  (:require [taoensso.timbre :as timbre]))


#?(:cljs
   (defn track-cmp [c-name c & args]
     (timbre/debug (str "tracker[" c-name "]:mount"))
     (let [state (atom nil)
           f (fn x [path v]
               (cond
                 (map? v)
                 (doseq [[k v] v]
                   (x (conj path k) v))

                 (or (seq? v) (vector? v))
                 (doseq [[i v] (map-indexed (fn [i v] [i v]) v)]
                   (x (conj path i) v))

                 :else (swap! state update-in path
                              (fn [o]
                                (when-not (= o v)
                                  (timbre/debug (str (pr-str path) ":\n\told:" (pr-str o) "\n\tnew:" (pr-str v))))
                                v))))]
       (fn [c-name c & args]
         (timbre/debug (str "tracker[" c-name "]:render"))
         (f [] args)
         ; return comp
         (into [c] args)))))

(defmacro track [c & args]
  (into ['hypercrud.ui.tracker/track-cmp (str c) c] args))
