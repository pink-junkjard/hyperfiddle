(ns hyperfiddle.ide.fiddles.domain
  (:require
    [contrib.reagent :refer [from-react-context]]))


(def ^:export domain-ident-renderer
  (from-react-context
    (fn [{:keys [ctx props]} value]
      (let [href (str "http://" value "." (get-in ctx [:host-env :ide/root]))]
        [:div
         [:a (merge props {:href href}) href]]))))
