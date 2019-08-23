(ns hyperfiddle.foundation
  (:require
    [cats.monad.either :as either]
    [hyperfiddle.project :as project]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security.domains]
    #?(:cljs [hyperfiddle.ui.iframe :as iframe])))


(def root-pid "root")

#?(:cljs
   (defn view [ctx]
     [:<>
      [:style {:dangerouslySetInnerHTML {:__html (:project/css (runtime/get-project (:runtime ctx) (:partition-id ctx)))}}]
      (-> (runtime/get-project (:runtime ctx) (:partition-id ctx))
          :project/code
          project/eval-domain-code!+
          (either/branch
            (fn [e] [:div [:h2 {:style {:margin-top "10%" :text-align "center"}} "Misconfigured domain"]])
            (fn [_] [iframe/iframe-cmp ctx])))]))
