(ns hyperfiddle.ide.fiddles.topnav
  (:require
    [cats.core :refer [fmap]]
    [cats.monad.either :as either]
    [clojure.string :as string]
    [contrib.ct :refer [unwrap]]
    [contrib.reactive :as r]
    [contrib.reader :refer [read-edn-string!]]
    [contrib.reagent :refer [fragment]]
    [contrib.rfc3986 :refer [encode-rfc3986-pchar]]
    [contrib.ednish :refer [encode-ednish]]
    [contrib.ui :refer [radio-option easy-checkbox]]
    [contrib.ui.tooltip :refer [tooltip]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.router :as router]
    [hyperfiddle.ide.system-fiddle :as system-fiddle]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.data]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.fiddle :as fiddle]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security.client :as security]
    [hyperfiddle.ui :as ui :refer [ui-from-link markdown]]
    [hyperfiddle.ui.util :refer [on-change->tx with-tx!]]
    [taoensso.timbre :as timbre]))


; inline sys-link data when the entity is a system-fiddle
(letfn [(-shadow-fiddle [target-ident fiddle-val]
          (cond
            (system-fiddle/system-fiddle? target-ident) (->> (system-fiddle/hydrate-system-fiddle target-ident)
                                                             (fmap #(fiddle/fiddle-defaults % nil))
                                                             (unwrap #(timbre/error %)))

            (nil? (:db/id fiddle-val)) fiddle-val
            :else (fiddle/fiddle-defaults fiddle-val nil)))]
  (defn shadow-fiddle [ctx]
    {:pre [(-> ctx :hypercrud.browser/data)]}
    (let [route @(:hypercrud.browser/route ctx)
          [_ [e]] route                                     ; [:hyperfiddle/topnav [#entity["$" [:fiddle/ident :hyperfiddle.system/remove]]]]
          [_ target-fiddle-ident] (:db/id e)]
      (update ctx :hypercrud.browser/data (partial r/fmap (r/partial -shadow-fiddle target-fiddle-ident))))))

(defn any-loading? [peer]
  (some (comp not nil? :hydrate-id val) @(runtime/state peer [::runtime/partitions])))

(defn loading-spinner [ctx & [?class]]
  (if @(r/track any-loading? (:peer ctx))
    [:div.display-inline-flex [re-com.core/throbber]]))

(defn src-mode? [frag]
  (= :src (some-> frag read-edn-string!)))

(defn- set-managed [link] (assoc link :link/tx-fn "(assert false \"Never called, triggers popover view, also this is the same bit of info as dont-branch\")"))

(defn renderer [val ctx props]
  (let [display-mode @(runtime/state (:peer ctx) [:display-mode])
        {:keys [hypercrud.browser/data] :as ctx} (shadow-fiddle ctx)
        ; hack until hyperfiddle.net#156 is complete
        fake-managed-anchor (fn [rel class ctx & [?label props]]
                              (let [link-ref (->> (unwrap #(timbre/error %) (hyperfiddle.data/select+ ctx rel class))
                                                  (r/fmap set-managed))]
                                [ui-from-link link-ref ctx (assoc props :dont-branch? true) ?label]))]
    [:div props
     [:div.left-nav
      [tooltip {:label "Home"} [:a.hf-auto-nav {:href "/"} @(runtime/state (:peer ctx) [::runtime/domain :domain/ident])]]
      [tooltip {:label "This fiddle"}                       ; also a good place for the route
       [:span.hf-auto-nav (some-> @(r/cursor (:hypercrud.browser/data ctx) [:fiddle/ident]) str)]]
      (fake-managed-anchor :hf/iframe :fiddle-shortcuts ctx "shortcuts" {:tooltip [nil "Fiddles in this domain"]})]

     [:div.right-nav {:key "right-nav"}                     ; CAREFUL; this key prevents popover flickering

      [loading-spinner ctx]

      (let [target-route (context/target-route ctx)
            src-mode (src-mode? (get target-route 3))
            no-target-fiddle (nil? (:db/id @data))          ; ide-route omits fiddle for ide routes
            change! #(runtime/dispatch! (:peer ctx) (actions/set-display-mode %))
            value (if src-mode :src display-mode)]
        [:span.radio-group
         (radio-option {:label "api" :tooltip "What the API client sees" :target :hypercrud.browser.browser-ui/api :change! change! :value value
                        :disabled (or src-mode no-target-fiddle)})
         (radio-option {:label "data" :tooltip "Ignore :fiddle/renderer" :target :hypercrud.browser.browser-ui/xray :change! change! :value value
                        :disabled (or src-mode no-target-fiddle)})
         (radio-option {:label "view" :tooltip "Use :fiddle/renderer" :target :hypercrud.browser.browser-ui/user :value value :change! change!
                        :disabled (or src-mode no-target-fiddle)})
         (radio-option {:label (let [root-rel-path (runtime/encode-route (:peer ctx) (router/dissoc-frag target-route))
                                     href (if-not src-mode
                                            (str root-rel-path "#" (encode-rfc3986-pchar (encode-ednish (pr-str :src))))
                                            (str root-rel-path "#"))]
                                 (if (and (not src-mode) (not no-target-fiddle))
                                   [:a {:href href :target "_blank"} "src"]
                                   [:span "src"]))
                        :tooltip "View fiddle source" :target :hypercrud.browser.browser-ui/src :value value :change! change!
                        :disabled (or (not src-mode) no-target-fiddle)})])
      (let [tooltip [:div {:style {:text-align "left"}}
                     [markdown
                      (->> @(runtime/state (:peer ctx) [::runtime/domain :domain/databases])
                           (reduce (fn [acc {:keys [:domain.database/name :domain.database/record]}]
                                     (if (contains? acc (:database/uri record))
                                       (update acc (:database/uri record) conj name)
                                       (assoc acc (:database/uri record) [name])))
                                   {@(runtime/state (:peer ctx) [::runtime/domain :domain/fiddle-database :database/uri]) ["Source"]
                                    ; domains-uri shouldn't need to be accessed
                                    foundation/domain-uri ["Domains"]})
                           (map (fn [[uri dbnames]]
                                  (let [prefix (if @(runtime/state (:peer ctx) [::runtime/auto-transact uri])
                                                 "- [x] "
                                                 "- [ ] ")]
                                    (str prefix (string/join "/" dbnames) " (" uri ")"))))
                           (string/join "\n")
                           (str "##### Auto-transact:\n\n"))
                      {:hyperfiddle.ui.markdown-extensions/unp true}]]
            dirty? (not @(r/fmap empty? (runtime/state (:peer ctx) [::runtime/partitions nil :stage])))]
        (fake-managed-anchor :hf/iframe :stage ctx "stage" {:tooltip [nil tooltip] :class (when dirty? "stage-dirty")}))
      (ui/link :hf/self :new-fiddle ctx "new-fiddle" (let [hf-db @(hyperfiddle.runtime/state (:peer ctx) [:hyperfiddle.runtime/domain :domain/fiddle-database])
                                                           subject @(hyperfiddle.runtime/state (:peer ctx) [:hyperfiddle.runtime/user-id])
                                                           writes-allowed?+ (security/subject-can-transact? hf-db subject)
                                                           anonymous? (nil? subject)]
                                                       {:disabled (either/branch writes-allowed?+ (constantly true) not) ; todo this logic could be factored out into ui-from-link
                                                        :tooltip (either/branch
                                                                   writes-allowed?+
                                                                   (fn [e] [:warning "Misconfigured db security"])
                                                                   (fn [writes-allowed?]
                                                                     (cond (and anonymous? (not writes-allowed?)) [:warning "Please login"]
                                                                           (not writes-allowed?) [:warning "Writes restricted"])))}))
      [tooltip {:label "Domain administration"} (ui/link :hf/self :domain ctx "domain")]
      (if @(runtime/state (:peer ctx) [::runtime/user-id])
        (let [{:keys [:hypercrud.browser/data]} (hyperfiddle.data/browse ctx :hf/iframe :account)]
          (fake-managed-anchor :hf/iframe :account ctx @(r/fmap :user/name data)
                               {:tooltip [nil @(r/fmap :user/email data)]}))
        [:a {:href (foundation/stateless-login-url ctx)} "login"])]]))

(defn ^:export qe-picker-control [val ctx props]
  (let [options (->> [:query :entity :blank]
                     (map #(radio-option
                             {:label (case % :query "query" :entity "pull" :blank "blank")
                              :target %
                              :value val
                              :change! (r/comp (r/partial with-tx! ctx)
                                               (r/partial on-change->tx ctx val))})))]
    [:span.qe.radio-group props
     (apply fragment options)]))

(letfn [(toggle-auto-transact! [ctx selected-uri]
          (runtime/dispatch! (:peer ctx) [:toggle-auto-transact @selected-uri]))]
  (defn ^:export stage-ui-buttons [selected-uri stage ctx]
    (let [writes-allowed?+ (let [hf-db (domain/uri->hfdb @selected-uri @(runtime/state (:peer ctx) [::runtime/domain]))
                                 subject @(runtime/state (:peer ctx) [::runtime/user-id])]
                             (security/subject-can-transact? hf-db subject))
          anonymous? (nil? @(runtime/state (:peer ctx) [::runtime/user-id]))]
      (fragment
        [tooltip (either/branch
                   writes-allowed?+
                   (fn [e] {:status :warning :label "Misconfigured db security"})
                   (fn [writes-allowed?]
                     (cond (and (not writes-allowed?) anonymous?) {:status :warning :label "Please login"}
                           (not writes-allowed?) {:status :warning :label "Writes restricted"}
                           (empty? @stage) {:status :warning :label "no changes"})))
         (let [disabled? (either/branch
                           writes-allowed?+
                           (constantly true)
                           (fn [writes-allowed?] (or (not writes-allowed?) (empty? @stage))))]
           [:button {:disabled disabled?
                     :style (if disabled? {:pointer-events "none"})
                     :on-click (fn []
                                 (let [invert-route (:hypercrud.browser/invert-route ctx)
                                       ; specifically dont use the SplitRuntime protocol here. the only thing that makes sense is whats in the context from the route
                                       nil-branch-aux {:hyperfiddle.ide/foo "page"}
                                       action (actions/manual-transact-uri! (:peer ctx) invert-route nil-branch-aux @selected-uri)]
                                   (runtime/dispatch! (:peer ctx) action)))}
            "transact!"])]
        " "
        [tooltip (either/branch
                   writes-allowed?+
                   (fn [e] {:status :warning :label "Misconfigured db security"})
                   (fn [writes-allowed?]
                     (cond (and anonymous? (not writes-allowed?)) {:status :warning :label "Please login"}
                           (not writes-allowed?) {:status :warning :label "Writes restricted"}
                           (not (empty? @stage)) {:status :warning :label "please transact! all changes first"})))
         (let [is-disabled (either/branch
                             writes-allowed?+
                             (constantly true)
                             (fn [writes-allowed?]
                               (or (not writes-allowed?) (not (empty? @stage)))))
               is-auto-transact @(runtime/state (:peer ctx) [::runtime/auto-transact @selected-uri])]
           [easy-checkbox {:disabled is-disabled
                           :style (if is-disabled {:pointer-events "none"})
                           :checked is-auto-transact
                           :on-change (r/partial toggle-auto-transact! ctx selected-uri)}
            "auto-transact"])]))))
