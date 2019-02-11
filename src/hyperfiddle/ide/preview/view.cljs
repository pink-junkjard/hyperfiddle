(ns hyperfiddle.ide.preview.view
  (:require
    [cats.monad.either :as either]
    [contrib.cljs-platform :refer [code-for-browser]]
    [contrib.css :refer [css]]
    [contrib.reactive :as r]
    [contrib.ui]
    [hypercrud.browser.base :as base]
    [hypercrud.util.branch :as branch]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.ide.domain :as ide-domain]
    [hyperfiddle.ide.preview.io :refer [->IOImpl]]
    [hyperfiddle.ide.preview.runtime :refer [->Runtime]]
    [hyperfiddle.ide.preview.state :refer [->FAtom]]
    [hyperfiddle.reducers :as reducers]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.iframe :refer [iframe-cmp]]
    [hyperfiddle.ui.loading :refer [loading-page]]
    [reagent.core :as reagent]
    [promesa.core :as p]))


(defn build-user-branch [ide-branch] (branch/encode-branch-child ide-branch "user"))

(def keypress (code-for-browser (js/keypress.Listener. js/document.body)))

(defn frame-on-click [rt route event]
  (when (and route (.-altKey event))                        ; under what circumstances is route nil?
    (let [native-event (.-nativeEvent event)
          anchor (-> (.composedPath native-event) (aget 0) (.matches "a"))
          anchor-descendant (-> (.composedPath native-event) (aget 0) (.matches "a *"))]
      (when-not (or anchor anchor-descendant)
        (.stopPropagation event)
        (js/window.open (domain/url-encode (runtime/domain rt) route) "_blank")))))

(defn with-rt [& args]
  (let [initial-render (r/atom true)
        is-refreshing (r/atom true)
        alt-key (r/atom false)
        display-mode (r/atom :hypercrud.browser.browser-ui/user)
        refresh! (fn [rt user-branch]
                   (reset! is-refreshing true)
                   (-> (actions/hydrate-partition rt user-branch (partial runtime/dispatch! rt) (fn [] @(runtime/state rt)))
                       (p/finally (fn [] (reset! is-refreshing false)))))]
    (reagent/create-class
      {:reagent-render
       (fn [rt route user-branch]
         [:div.result.col-sm
          [:div.url-toolbar
           ; todo animation
           [:button {:class "refresh" :on-click #(refresh! rt user-branch) :disabled @is-refreshing} "↻"]
           [contrib.ui/text
            {:value (domain/url-encode (runtime/domain rt) route)
             :disabled true
             :class "url"
             :on-change (fn [s] (println s))}]
           (into [:span]
                 (->> [{:label "edn" :tooltip "What the API client sees" :value :hypercrud.browser.browser-ui/api}
                       {:label "data" :tooltip "Ignore :fiddle/renderer" :value :hypercrud.browser.browser-ui/xray}
                       {:label "view" :tooltip "Use :fiddle/renderer" :value :hypercrud.browser.browser-ui/user}]
                      (map (fn [props]
                             [contrib.ui/radio-with-label
                              (assoc props
                                :checked (= (:value props) @display-mode)
                                :on-change (r/partial reset! display-mode))]))))]
          (if @initial-render
            [loading-page]
            (let [ctx {:peer rt
                       :branch user-branch
                       :hyperfiddle.ui/debug-tooltips true
                       :hypercrud.ui/display-mode display-mode
                       :hyperfiddle.ui.iframe/on-click (r/partial frame-on-click rt)}]
              [iframe-cmp ctx
               {:route route
                :class (css "hyperfiddle-user"
                            "hyperfiddle-ide"
                            "hf-live"
                            (when @alt-key "alt")
                            (some-> @display-mode name (->> (str "display-mode-"))))}]))])

       :component-did-mount
       (fn [this]
         (let [[_ rt route user-branch] (reagent/argv this)]
           (.simple-combo keypress "ctrl `" (fn []
                                              (swap! display-mode #(if (not= :hypercrud.browser.browser-ui/user %)
                                                                     :hypercrud.browser.browser-ui/user
                                                                     :hypercrud.browser.browser-ui/xray))))

           (.register-combo keypress #js {"keys" "alt"
                                          "prevent_repeat" true
                                          "on_keydown" #(reset! alt-key true)
                                          "on_keyup" #(reset! alt-key false)})

           (-> (foundation/bootstrap-data2 rt foundation/LEVEL-NONE foundation/LEVEL-HYDRATE-PAGE route user-branch nil)
               (p/finally (fn []
                            (reset! initial-render false)
                            (reset! is-refreshing false))))))

       :component-did-update
       (fn [this [_ _ prev-route _]]
         (let [[_ rt next-route user-branch] (reagent/argv this)]
           (when-not (= prev-route next-route)
             (reset! is-refreshing true)
             (-> (foundation/bootstrap-data2 rt foundation/LEVEL-GLOBAL-BASIS foundation/LEVEL-HYDRATE-PAGE next-route user-branch nil)
                 (p/finally (fn [] (reset! is-refreshing false)))))))

       :component-will-unmount
       (fn [this]
         (.reset keypress))
       })))

(defn- to [parent-state user-state]
  (let [user-state (-> (update user-state ::runtime/partitions dissoc nil)
                       (dissoc ::runtime/user-id)
                       (reducers/root-reducer nil))]
    (assoc parent-state ::runtime/user-state user-state)))

(defn- from [ide-branch parent-state]
  (let [nil-partition (-> (get-in parent-state [::runtime/partitions ide-branch])
                          (select-keys [:route :stage])
                          (update :stage (fn [stage] {'hyperfiddle.domain/fiddle-database (get stage "$")})))]
    (-> (::runtime/user-state parent-state)
        (assoc ::runtime/user-id (::runtime/user-id parent-state))
        (assoc-in [::runtime/partitions nil] nil-partition)
        (reducers/root-reducer nil))))

(defn view-cmp [user-domain-record ctx props]
  (let []
    (fn [user-domain-record ctx props]
      (either/branch
        (ide-domain/build-user+ (runtime/domain (:peer ctx)) user-domain-record)
        (fn [e]
          [:div
           [:pre (js/pprint-str user-domain-record)]
           [foundation/error-cmp e]])
        (fn [user-domain]
          (let [ide-branch (:branch ctx)
                user-route (let [[_ [fiddle-lookup-ref & datomic-args] service-args encoded-fragment] @(runtime/state (:peer ctx) [::runtime/partitions ide-branch :route])]
                             (route/canonicalize
                               (base/legacy-lookup-ref->fiddle-ident fiddle-lookup-ref)
                               (vec datomic-args)
                               service-args
                               (when-not (hyperfiddle.ide/parse-ide-fragment encoded-fragment)
                                 encoded-fragment)))
                user-state (->FAtom (runtime/state (:peer ctx)) to (r/partial from ide-branch))
                user-io (->IOImpl user-domain)
                user-runtime (->Runtime user-domain user-io user-state reducers/root-reducer)]
            [with-rt user-runtime user-route (build-user-branch ide-branch)]))))))
