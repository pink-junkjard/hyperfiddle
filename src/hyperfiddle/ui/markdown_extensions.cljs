(ns hyperfiddle.ui.markdown-extensions)


(defn a [content argument props ctx]
  [:a (merge {:href argument} (dissoc props :children))
   ; backwards compat with vanilla markdown anchors
   (or (:children props) content)])
