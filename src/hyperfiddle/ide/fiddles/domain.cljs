(ns hyperfiddle.ide.fiddles.domain)


(defn ^:export domain-ident-renderer [val props ctx]
  (let [href (str "http://" val "." (get-in ctx [:host-env :ide/root]))]
    [:div
     [:a (merge props {:href href}) href]]))
