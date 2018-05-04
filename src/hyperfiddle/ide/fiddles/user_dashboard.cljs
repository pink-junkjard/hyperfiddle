(ns hyperfiddle.ide.fiddles.user-dashboard
  (:require [contrib.reactive :as r]
            [hypercrud.ui.control.markdown-rendered :refer [markdown]]
            [hypercrud.ui.native-event-listener :refer [native-on-click-listener]]
            [hyperfiddle.ide.actions :as ide-actions]
            [hyperfiddle.runtime :as runtime]))


(defn logout! [rt e]
  (runtime/dispatch! rt (ide-actions/set-user-profile rt nil))
  (.preventDefault e)
  (.stopPropagation e))

(defn renderer [ctx]
  [:div.hyperfiddle-user-dashboard
   (if (-> ctx :user-profile)
     [native-on-click-listener
      {:key :logout :on-click (r/partial logout! (:peer ctx))}
      ; todo https://auth0.com/docs/logout
      [:span.nav-link.auth {:key :logout}
       [:a {:href "/logout"} "logout"]]])
   [hypercrud.ui.control.markdown-rendered/markdown (some-> ctx :hypercrud.browser/fiddle deref :fiddle/markdown) ctx]
   [:ul.link-list
    (->> @(:hypercrud.browser/result ctx)
         (sort-by :domain/ident)
         (map (fn [domain]
                [:li {:key (hash (:db/id domain))}
                 [:a {:href (str "http://" (:domain/ident domain) "." (:hyperfiddle-hostname ctx) "/")} (:domain/ident domain)]]))
         (doall))]])
