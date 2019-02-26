(ns hyperfiddle.ide.preview.view
  (:require
    [cats.monad.either :as either]
    [contrib.cljs-platform :refer [code-for-browser]]
    [contrib.component :as component]
    [contrib.css :refer [css]]
    [contrib.data :refer [map-values]]
    [contrib.reactive :as r]
    [contrib.ui]
    [hypercrud.browser.base :as base]
    [hypercrud.ui.error :as error-cmps]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.branch :as branch]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.ide.domain :as ide-domain]
    [hyperfiddle.ide.preview.io :refer [->IOImpl]]
    [hyperfiddle.ide.preview.runtime :refer [->Runtime]]
    [hyperfiddle.ide.preview.state :refer [->FAtom]]
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
        initial-render (r/atom true)
        is-refreshing (r/atom true)
        is-hovering-refresh-button (r/atom false)
        alt-key (r/atom false)
        display-mode (r/atom :hypercrud.browser.browser-ui/user)
        ; specifically deref and re-wrap this ref on mount because we are tracking deviation from this value
        staleness (r/atom (ide-branch-reference rt ide-branch))
        is-stale? (fn [] (not= @staleness (ide-branch-reference rt ide-branch)))
        refresh! (fn []
                   (reset! staleness (ide-branch-reference rt ide-branch))
                   (reset! is-refreshing true)
                   (-> (actions/hydrate-partition (runtime/io rt) user-branch (partial runtime/dispatch! rt) (fn [] @(runtime/state rt)))
                       (p/finally (fn [] (reset! is-refreshing false)))))]
    (reagent/create-class
      {:reagent-render
       (fn [rt route ide-branch]
         (let [ctx {:peer rt
                    :branch user-branch
                    :hyperfiddle.ui/debug-tooltips true
                    :hypercrud.ui/display-mode display-mode
                    :hyperfiddle.ui.iframe/on-click (r/partial frame-on-click rt)}]
           [:<>
            (let [stale @(r/track is-stale?)]
              [:div.preview-toolbar (when stale {:class "stale"})
               ; todo animation
               [:button {:class "refresh" :on-click #(refresh!)
                         :disabled @is-refreshing
                         :on-mouse-over #(do (reset! is-hovering-refresh-button true) nil)
                         :on-mouse-out #(do (reset! is-hovering-refresh-button false) nil)}
                [re-com/popover-tooltip
                 :showing? (r/atom (and @is-hovering-refresh-button (not @is-refreshing)))
                 :anchor "↻"
                 :label (if stale
                          "Currently stale, refresh to see changes (or press alt+enter)"
                          "Refresh preview to see changes")]]
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
                                    :checked (= (:value props) @display-mode)
                                    :on-change (r/partial reset! display-mode))]))))
               [staging/popover-button rt user-branch (staging/default-dbname-labels rt)]])
            (if @initial-render
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
                 [:div#root (when @alt-key {:style {:cursor "pointer"}})
                  [:style {:dangerouslySetInnerHTML {:__html @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :project :project/css])}}]
                  ^{:key "user-iframe"}
                  [iframe-cmp ctx
                   {:route route
                    :class (css "hyperfiddle-user"
                                "hyperfiddle-ide"
                                (some-> @display-mode name (->> (str "display-mode-"))))}]]]))]))

       :component-did-mount
       (fn [this]
         (let [[_ rt route ide-branch] (reagent/argv this)]
           (.register-many keypress (clj->js [{"keys" "alt"
                                               "prevent_repeat" true
                                               "on_keydown" #(reset! alt-key true)
                                               "on_keyup" #(reset! alt-key false)}
                                              {"keys" "alt enter"
                                               "on_keydown" #(refresh!)}
                                              {"keys" "ctrl `"
                                               "on_keydown" (fn []
                                                              (swap! display-mode #(if (not= :hypercrud.browser.browser-ui/user %)
                                                                                     :hypercrud.browser.browser-ui/user
                                                                                     :hypercrud.browser.browser-ui/xray)))}]))
           (runtime/dispatch! rt [:partition-route user-branch route])
           (swap! ls component/start)
           (-> (actions/bootstrap-data rt user-branch actions/LEVEL-NONE)
               (p/finally (fn []
                            (reset! initial-render false)
                            (reset! is-refreshing false))))))

       :component-did-update
       (fn [this [_ _ prev-route _]]
         (let [[_ rt next-route ide-branch] (reagent/argv this)]
           (when-not (= prev-route next-route)
             (reset! is-refreshing true)
             (runtime/dispatch! rt (fn [dispatch! get-state]
                                     (-> (actions/set-route rt next-route user-branch false dispatch! get-state)
                                         (p/finally (fn [] (reset! is-refreshing false)))))))))

       :component-will-unmount
       (fn [this]
         (component/stop @ls)
         (.reset keypress))
       })))

(defn- to [parent-state user-state]
  (let [user-state (-> (update user-state ::runtime/partitions dissoc foundation/root-branch) ; root branch is readonly to users
                       (dissoc ::runtime/user-id)
                       (reducers/root-reducer nil))]
    (assoc parent-state ::runtime/user-state user-state)))

(defn- from [ide-domain ide-branch parent-state]
  (let [root-partition (-> (get-in parent-state [::runtime/partitions ide-branch])
                           (select-keys [:route :stage :local-basis])
                           (update :stage (fn [stage] (map-values #(get stage %) (::ide-domain/user-dbname->ide ide-domain)))))]
    (-> (::runtime/user-state parent-state)
        (assoc ::runtime/user-id (::runtime/user-id parent-state))
        (assoc-in [::runtime/partitions foundation/root-branch] root-partition)
        (update-in [::runtime/partitions (build-user-branch-id ide-branch)] #(or % {})) ; branch MUST exist in state
        (reducers/root-reducer nil))))

(defn view-cmp [user-domain-record ctx props]
  (let []
    (fn [user-domain-record ctx props]
      [:div (select-keys props [:class])
       (either/branch
         (ide-domain/build-user+ (runtime/domain (:peer ctx)) user-domain-record)
         (fn [e]
           [:<>
            [:h2 "Domain misconfigured"]                    ; todo improve me
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
             [with-rt user-runtime user-route ide-branch])))])))
