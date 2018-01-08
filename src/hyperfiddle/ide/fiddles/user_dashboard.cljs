(ns hyperfiddle.ide.fiddles.user-dashboard
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [hypercrud.ui.markdown :refer [markdown]]
            [hypercrud.ui.native-event-listener :refer [native-listener]]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.util.string :as hc-string]
            [hyperfiddle.appval.state.actions :as foundation-actions]))


(defn logout! [rt dispatch! e]
  (dispatch! (foundation-actions/set-user-profile rt nil))
  (.preventDefault e)
  (.stopPropagation e))

(defn renderer [domains ordered-fes anchors ctx]
  [:div.hyperfiddle-user-dashboard
   (if (-> ctx :user-profile)
     [native-listener
      {:key :logout :on-click (reactive/partial logout! (:peer ctx) (:dispatch! ctx))}
      ; todo https://auth0.com/docs/logout
      [:span.nav-link.auth {:key :logout}
       [:a {:href "/logout"} "logout"]]])
   [markdown (-> ctx :fiddle :db/doc)]
   [:ul.link-list
    (->> domains
         (sort-by :domain/ident)
         (map (fn [domain]
                [:li {:key (hash (:db/id domain))}
                 [(:navigate-cmp ctx) {:external-hostname (str (:domain/ident domain) "." (:hyperfiddle-hostname ctx))
                                       :route (-> (:domain/home-route domain)
                                                  (hc-string/memoized-safe-read-edn-string)
                                                  (cats/mplus (either/right nil))
                                                  (cats/extract))}
                  (:domain/ident domain)]]))
         (doall))]
   [:style {:dangerouslySetInnerHTML {:__html "
.hyperfiddle-user-dashboard { width: 250px; }
"}}]])
