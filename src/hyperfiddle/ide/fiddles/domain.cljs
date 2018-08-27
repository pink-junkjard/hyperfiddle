(ns hyperfiddle.ide.fiddles.domain
  (:require
    [hyperfiddle.runtime :as runtime]))


(defn ^:export domain-ident-renderer [val ctx props]
  (let [href (str "http://" val "." (:ide/root (runtime/host-env (:peer ctx))))]
    [:div
     [:a (merge props {:href href}) href]]))
