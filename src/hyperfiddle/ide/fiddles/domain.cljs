(ns hyperfiddle.ide.fiddles.domain)


(defn ^:export domain-ident-renderer [ref props ctx]
  (let [href (str "http://" @ref "." (get-in ctx [:host-env :ide/root]))]
    [:div
     [:a (merge props {:href href}) href]]))
