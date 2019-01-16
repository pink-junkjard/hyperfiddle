(ns hyperfiddle.ide.preview.view
  (:require
    [cats.monad.either :as either]
    [contrib.reactive :as r]
    [hypercrud.browser.base :as base]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.domains.ednish :as ednish]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.ide.preview.io :refer [->IOImpl]]
    [hyperfiddle.ide.preview.runtime :refer [->Runtime]]
    [hyperfiddle.reducers :as reducers]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.iframe :refer [iframe-cmp]]))


#_(foundation/bootstrap-data rt foundation/LEVEL-NONE load-level path)

(defn view-cmp [user-domain-record ctx props]
  (let []
    (fn [user-domain-record ctx props]
      (either/branch
        (ednish/build+ user-domain-record)
        (fn [e] [:h2 "user-domain misconfigured :("])                                 ; todo
        (fn [user-domain]
          (let [user-route (let [[_ [fiddle-lookup-ref] user-args encoded-fragment] @(runtime/state (:peer ctx) [::runtime/partitions nil :route])]
                             (into [(base/legacy-lookup-ref->fiddle-ident fiddle-lookup-ref)] user-args))
                user-state (-> {}
                               (reducers/root-reducer nil)
                               r/atom)
                user-io (let [ide-io (runtime/io (:peer ctx))
                              ; this is hacky
                              service-uri (.-service-uri ide-io)
                              build (.-build ide-io)]
                          (->IOImpl user-domain service-uri build))
                user-runtime (->Runtime user-domain user-io user-state reducers/root-reducer)
                user-ctx {:peer user-runtime}]
            [:div.result.col-sm
             [:div.url-toolbar
              [:button {:class "refresh"} "↻"]
              [:input.url {:type "text" :value (domain/url-encode user-domain user-route)}]
              (into [:span]
                    (->> [{:label "edn" :tooltip "What the API client sees" :value :hypercrud.browser.browser-ui/api}
                          {:label "data" :tooltip "Ignore :fiddle/renderer" :value :hypercrud.browser.browser-ui/xray}
                          {:label "view" :tooltip "Use :fiddle/renderer" :value :hypercrud.browser.browser-ui/user}]
                         (map (fn [props]
                                [contrib.ui/radio-with-label
                                 (assoc props
                                   :checked (= (:value props) @(runtime/state user-runtime [:display-mode]))
                                   :on-change (r/comp (r/partial hyperfiddle.runtime/dispatch! user-runtime) hyperfiddle.actions/set-display-mode))]))))]
             [iframe-cmp user-ctx {:route user-route}]])))
      )))
