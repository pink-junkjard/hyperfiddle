(ns hyperfiddle.ide.preview.view
  (:require
    [cats.monad.either :as either]
    [contrib.cljs-platform :refer [code-for-browser]]
    [contrib.component :as component]
    [contrib.css :refer [css]]
    [contrib.data :refer [map-values]]
    [contrib.local-storage :as local-storage]
    [contrib.reactive :as r]
    [contrib.ui]
    [hypercrud.browser.base :as base]
    [hypercrud.ui.error :as error-cmps]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.branch :as branch]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.ide.domain :as ide-domain]
    [hyperfiddle.ide.preview.runtime :refer [->Runtime]]
    [hyperfiddle.ide.preview.state :refer [->FAtom]]
    [hyperfiddle.io.basis :as basis]
    [hyperfiddle.io.browser :refer [->IOImpl]]
    [hyperfiddle.local-storage-sync :refer [map->LocalStorageSync]]
    [hyperfiddle.project :as project]
    [hyperfiddle.reducers :as reducers]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.iframe :refer [iframe-cmp]]
    [hyperfiddle.ui.loading :refer [loading-page]]
    [hyperfiddle.ui.staging :as staging]
    [re-com.core :as re-com]
    [reagent.core :as reagent]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(def keypress (code-for-browser (js/keypress.Listener. js/document.body)))

(defn frame-on-click [rt route event]
  (when (and route (.-altKey event))                        ; under what circumstances is route nil?
    (let [native-event (.-nativeEvent event)
          anchor (-> (.composedPath native-event) (aget 0) (.matches "a"))
          anchor-descendant (-> (.composedPath native-event) (aget 0) (.matches "a *"))]
      (when-not (or anchor anchor-descendant)
        (.stopPropagation event)
        (js/window.open (domain/url-encode (runtime/domain rt) route) "_blank")))))

(defn build-user-branch-id [ide-branch] (branch/child-branch-id ide-branch "user"))

(defn ide-branch-reference [rt ide-branch]
  (-> @(runtime/state rt [::runtime/partitions ide-branch])
      (select-keys [:stage :local-basis])))

(defn with-rt [rt route ide-branch]
  (let [user-branch (build-user-branch-id ide-branch)
        ls (atom (map->LocalStorageSync {:rt rt :branch-id user-branch :ls-key :USER-STATE}))
        preview-state (r/atom {:initial-render true
                               :is-refreshing true
                               :is-hovering-refresh-button false
                               :alt-key-pressed false
                               :display-mode :hypercrud.browser.browser-ui/user

                               ; specifically deref and re-wrap this ref on mount because we are tracking deviation from this value
                               :staleness (ide-branch-reference rt ide-branch)
                               })
        stale-global-basis? (fn []
                              (= -1 (basis/compare-uri-maps @(runtime/state rt [::runtime/global-basis :user])
                                                            @(runtime/state rt [::ide-user-global-basis]))))
        stale-local-basis? (fn []
                             (= -1 (basis/compare-uri-maps
                                     @(runtime/state rt [::runtime/partitions user-branch :local-basis])
                                     @(runtime/state rt [::runtime/partitions ide-branch :local-basis]))))
        is-stale? (fn []
                    #_(println (pr-str {:initial-render @(r/cursor preview-state [:initial-render])
                                        :staleness-not= (not= @(r/cursor preview-state [:staleness]) (ide-branch-reference rt ide-branch))
                                        :stale-gb? (stale-global-basis?)
                                        :stale-lb (stale-local-basis?)}))
                    (and (not @(r/cursor preview-state [:initial-render]))
                         (or (not= @(r/cursor preview-state [:staleness]) (ide-branch-reference rt ide-branch))
                             (stale-global-basis?)
                             (stale-local-basis?))))
        refresh! (fn []
                   (when-not (or @(r/cursor preview-state [:is-refreshing])
                                 @(r/cursor preview-state [:initial-render]))
                     (let [init-level (cond
                                        (stale-global-basis?) actions/LEVEL-NONE
                                        (stale-local-basis?) actions/LEVEL-GLOBAL-BASIS
                                        :else actions/LEVEL-LOCAL-BASIS)]
                       (swap! preview-state assoc
                              :staleness (ide-branch-reference rt ide-branch)
                              :is-refreshing true)
                       (-> (actions/bootstrap-data rt user-branch init-level)
                           (p/finally (fn [] (swap! preview-state assoc :is-refreshing false)))))))]
    (reagent/create-class
      {:reagent-render
       (fn [rt route ide-branch]
         (let [ctx {:peer rt
                    :branch user-branch
                    :hyperfiddle.ui/debug-tooltips true
                    :hypercrud.ui/display-mode (r/cursor preview-state [:display-mode])
                    :hyperfiddle.ui.iframe/on-click (r/partial frame-on-click rt)}]
           [:<>
            (let [stale @(r/track is-stale?)]
              [:div.preview-toolbar (when stale {:class "stale"})
               ; todo animation
               (let [is-hovering-refresh-button (r/cursor preview-state [:is-hovering-refresh-button])]
                 [:button {:class "refresh" :on-click #(refresh!)
                           :disabled @(r/cursor preview-state [:is-refreshing])
                           :on-mouse-over #(do (reset! is-hovering-refresh-button true) nil)
                           :on-mouse-out #(do (reset! is-hovering-refresh-button false) nil)}
                  [re-com/popover-tooltip
                   :showing? (r/atom (and @is-hovering-refresh-button (not @(r/cursor preview-state [:is-refreshing]))))
                   :anchor "↻"
                   :label (if stale
                            "Currently stale, refresh to see changes (or press alt+enter)"
                            "Refresh preview to see changes")]])
               [:span.url]
               #_[contrib.ui/text
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
                                    :checked (= (:value props) @(r/cursor preview-state [:display-mode]))
                                    :on-change #(swap! preview-state assoc :display-mode %))]))))
               [staging/popover-button rt user-branch (staging/default-dbname-labels rt) :show-auto-tx false]])
            (if @(r/cursor preview-state [:initial-render])
              [loading-page]
              (let [code+ (project/eval-domain-code!+ @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :project :project/code]))]
                [:<>
                 (when (either/left? code+)
                   (let [e @code+]
                     (timbre/error e)
                     (let [href (domain/url-encode (runtime/domain (:peer ctx)) [:hyperfiddle.ide/env])
                           message (or (some-> (ex-cause e) ex-message) (ex-message e))]
                       [:h6 {:style {:text-align "center" :background-color "lightpink" :margin 0 :padding "0.5em 0"}}
                        "Exception evaluating " [:a {:href href} [:code ":domain/code"]] ": " message])))
                 [:div#root (when @(r/cursor preview-state [:alt-key-pressed]) {:style {:cursor "pointer"}})
                  [:style {:dangerouslySetInnerHTML {:__html @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :project :project/css])}}]
                  ^{:key "user-iframe"}
                  [iframe-cmp ctx
                   {:route route
                    :class (some-> @(r/cursor preview-state [:display-mode]) name (->> (str "display-mode-")))}]]]))]))

       :component-did-mount
       (fn [this]
         (let [[_ rt route ide-branch] (reagent/argv this)]
           (.register-many keypress (clj->js [{"keys" "alt"
                                               "prevent_repeat" true
                                               "on_keydown" #(swap! preview-state assoc :alt-key-pressed true)
                                               "on_keyup" #(swap! preview-state assoc :alt-key-pressed false)}
                                              {"keys" "alt enter"
                                               "on_keydown" #(refresh!)}
                                              {"keys" "ctrl `"
                                               "on_keydown" (fn []
                                                              (swap! preview-state update :display-mode
                                                                     #(if (not= :hypercrud.browser.browser-ui/user %)
                                                                        :hypercrud.browser.browser-ui/user
                                                                        :hypercrud.browser.browser-ui/xray)))}]))
           (runtime/dispatch! rt [:partition-route user-branch route])
           (when local-storage/is-supported
             (swap! ls component/start))
           (-> (actions/bootstrap-data rt user-branch actions/LEVEL-NONE)
               (p/finally (fn []
                            (swap! preview-state assoc
                                   :initial-render false
                                   :is-refreshing false))))))

       :component-did-update
       (fn [this [_ _ prev-route _]]
         (let [[_ rt next-route ide-branch] (reagent/argv this)]
           (when-not (= prev-route next-route)
             (swap! preview-state assoc :is-refreshing true)
             (runtime/dispatch! rt (fn [dispatch! get-state]
                                     (-> (actions/set-route rt next-route user-branch false dispatch! get-state)
                                         (p/finally (fn [] (swap! preview-state assoc
                                                                  :staleness (ide-branch-reference rt ide-branch)
                                                                  :is-refreshing false)))))))))

       :component-will-unmount
       (fn [this]
         (when local-storage/is-supported
           (component/stop @ls))
         (.reset keypress))
       })))

(defn- to [parent-state user-state]
  (let [user-state (-> (update user-state ::runtime/partitions dissoc foundation/root-branch) ; root branch is readonly to users
                       (dissoc ::ide-user-global-basis ::runtime/user-id)
                       (reducers/root-reducer nil))]
    (assoc parent-state ::runtime/user-state user-state)))

(defn- from [ide-domain ide-branch parent-state]
  (let [root-partition (-> (get-in parent-state [::runtime/partitions ide-branch])
                           (select-keys [:route :stage :local-basis])
                           (update :local-basis (fn [local-basis] (map-values #(get local-basis %) (::ide-domain/user-dbname->ide ide-domain))))
                           (update :stage (fn [stage] (map-values #(get stage %) (::ide-domain/user-dbname->ide ide-domain)))))]
    (-> (::runtime/user-state parent-state)
        (assoc ::ide-user-global-basis (map-values #(get-in parent-state [::runtime/global-basis :user %])
                                                   (::ide-domain/user-dbname->ide ide-domain))
               ::runtime/user-id (::runtime/user-id parent-state))
        (assoc-in [::runtime/partitions foundation/root-branch] root-partition)
        (update-in [::runtime/partitions (build-user-branch-id ide-branch)] #(or % {})) ; branch MUST exist in state
        (reducers/root-reducer nil))))

(defn view-cmp [user-domain-record ctx props]
  [:div (select-keys props [:class])
   (either/branch
     (ide-domain/build-user+ (runtime/domain (:peer ctx)) user-domain-record)
     (fn [e]
       [:<>
        [:h2 "Domain misconfigured"]                        ; todo improve me
        [error-cmps/error-block e]])
     (fn [user-domain]
       (let [ide-branch (:branch ctx)
             user-route (let [[_ [fiddle-lookup-ref & datomic-args] service-args encoded-fragment] @(runtime/state (:peer ctx) [::runtime/partitions ide-branch :route])]
                          (route/canonicalize
                            (base/legacy-lookup-ref->fiddle-ident fiddle-lookup-ref)
                            (vec datomic-args)
                            service-args
                            (when-not (hyperfiddle.ide/parse-ide-fragment encoded-fragment)
                              encoded-fragment)))
             user-state (->FAtom (runtime/state (:peer ctx)) to (r/partial from (runtime/domain (:peer ctx)) ide-branch))
             user-io (->IOImpl user-domain)
             user-runtime (->Runtime user-domain user-io user-state reducers/root-reducer)]
         [with-rt user-runtime user-route ide-branch])))])
