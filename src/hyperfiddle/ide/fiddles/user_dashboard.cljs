(ns hyperfiddle.ide.fiddles.user-dashboard
  (:require [hypercrud.ui.markdown :refer [markdown]]
            [hypercrud.ui.native-event-listener :refer [native-on-click-listener]]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.ide.actions :as ide-actions]
            [hyperfiddle.runtime :as runtime]))


(defn logout! [rt e]
  (runtime/dispatch! rt (ide-actions/set-user-profile rt nil))
  (.preventDefault e)
  (.stopPropagation e))

(defn renderer [domains ordered-fes anchors ctx]
  [:div.hyperfiddle-user-dashboard
   (if (-> ctx :user-profile)
     [native-on-click-listener
      {:key :logout :on-click (reactive/partial logout! (:peer ctx))}
      ; todo https://auth0.com/docs/logout
      [:span.nav-link.auth {:key :logout}
       [:a {:href "/logout"} "logout"]]])
   [markdown (-> ctx :fiddle :db/doc)]
   [:ul.link-list
    (->> domains
         (sort-by :domain/ident)
         (map (fn [domain]
                [:li {:key (hash (:db/id domain))}
                 [:a {:href (str "http://" (:domain/ident domain) "." (:hyperfiddle-hostname ctx) "/")} (:domain/ident domain)]]))
         (doall))]
   [:style {:dangerouslySetInnerHTML {:__html "
.hyperfiddle-user-dashboard { width: 250px; }
"}}]])
