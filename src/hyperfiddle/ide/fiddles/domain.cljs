(ns hyperfiddle.ide.fiddles.domain
  (:require
    [contrib.reagent :refer [from-react-context]]))


(defn domain-ident-renderer' [value ctx props]
  (let [href (str "http://" value "." (get-in ctx [:host-env :ide/root]))]
    [:a {:href href} href]))

(def ^:export domain-ident-renderer
  (from-react-context
    (fn [{:keys [ctx props]} value]
      ; I waffled between [] and () here; probably best
      ; to leave reactions to same scope as without this
      [domain-ident-renderer' value ctx props])))
