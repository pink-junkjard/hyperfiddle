(ns hyperfiddle.ui.navigate-cmp
  (:require
    [contrib.css :refer [css]]
    [contrib.eval :as eval]
    [contrib.keypress :refer [with-keychord]]
    [contrib.reactive :as r]
    [contrib.pprint :refer [pprint-str]]
    [contrib.try$ :refer [try-promise]]
    [contrib.ui.tooltip :refer [tooltip]]
    [cuerdas.core :as string]
    [hypercrud.browser.context :as context]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.runtime :as runtime]
    [promesa.core :as p]
    [re-com.core :as re-com]
    [taoensso.timbre :as timbre]))


(defn dissoc-non-native-props [hypercrud-props]
  (dissoc hypercrud-props :route :tooltip :popover :hidden))

(defn anchor-cmp [route-encode hypercrud-props label]
  {:pre [(not (:on-click hypercrud-props))]}
  (let [anchor-props (-> hypercrud-props
                         (dissoc-non-native-props)
                         (assoc :href (if (:route hypercrud-props)
                                        (route-encode (:route hypercrud-props))
                                        nil #_"javascript:void 0;")))]
    [:a anchor-props label]))

(let [safe-eval-string #(try-promise (eval/eval-string %))
      memoized-eval-string (memoize safe-eval-string)]
  (defn stage! [link-ref route popover-id child-branch ctx]
    (let [{:keys [:link/tx-fn] :as link} @link-ref]
      (-> (if (and (string? tx-fn) (not (string/blank? tx-fn)))
            (memoized-eval-string tx-fn)
            (p/resolved (constantly nil)))
          (p/then
            (fn [user-txfn]
              (p/promise
                (fn [resolve! reject!]
                  (let [swap-fn (fn [multi-color-tx]
                                  (let [result (let [result (user-txfn ctx multi-color-tx route)]
                                                 ; txfn may be sync or async
                                                 (if-not (p/promise? result) (p/resolved result) result))]
                                    ; let the caller of this :stage fn know the result
                                    ; This is super funky, a swap-fn should not be effecting, but seems like it would work.
                                    (p/branch result
                                              (fn [v] (resolve! nil))
                                              (fn [e]
                                                (reject! e)
                                                (timbre/warn e)))

                                    ; return the result to the action, it could be a promise
                                    result))]
                    (runtime/dispatch! (:peer ctx)
                                       (actions/stage-popover (:peer ctx) (:hypercrud.browser/invert-route ctx) child-branch
                                                              link swap-fn ; the swap-fn could be determined via the link rel
                                                              (actions/close-popover (:branch ctx) popover-id))))))))
          ; todo something better with these exceptions (could be user error)
          (p/catch (fn [err] (js/alert (pprint-str err))))))))

(defn close! [popover-id ctx]
  (runtime/dispatch! (:peer ctx) (actions/close-popover (:branch ctx) popover-id)))

(defn cancel! [popover-id child-branch ctx]
  (runtime/dispatch! (:peer ctx) (actions/batch
                                   (actions/close-popover (:branch ctx) popover-id)
                                   (actions/discard-partition child-branch))))

(defn managed-popover-body [route popover-id child-branch link-ref ctx]
  [:div.hyperfiddle-popover-body                            ; wrpaper helps with popover max-width, hard to layout without this
   ; NOTE: this ctx logic and structure is the same as the popover branch of browser-request/recurse-request
   (let [ctx (cond-> (context/clean ctx)                    ; hack clean for block level errors
               child-branch (assoc :branch child-branch))]
     [hypercrud.browser.core/ui-from-route route ctx])      ; cycle
   (when child-branch
     [:button {:on-click (r/partial stage! link-ref route popover-id child-branch ctx)} "stage"])
   (if child-branch
     [:button {:on-click #(cancel! popover-id child-branch ctx)} "cancel"]
     [:button {:on-click #(close! popover-id ctx)} "close"])])

(defn open! [route popover-id child-branch ctx]
  (runtime/dispatch! (:peer ctx)
                     (if child-branch
                       (actions/add-partition (:peer ctx) route child-branch (::runtime/branch-aux ctx)
                                              (actions/open-popover (:branch ctx) popover-id))
                       (actions/open-popover (:branch ctx) popover-id))))

(defn- show-popover? [popover-id ctx]
  (runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :popovers popover-id]))

(defn popover-cmp [hypercrud-props label ctx]
  (let [{:keys [child-branch popover-id link]} (:popover hypercrud-props)
        route (:route hypercrud-props)]
    [with-keychord
     "esc" #(do (js/console.warn "esc") (if child-branch
                                          (cancel! popover-id child-branch ctx)
                                          (close! popover-id ctx)))
     [re-com/popover-anchor-wrapper
      :showing? (show-popover? popover-id ctx)
      :position :below-center
      :anchor (let [btn-props (-> hypercrud-props
                                  (dissoc-non-native-props)
                                  (assoc :on-click (r/partial open! route popover-id child-branch ctx))
                                  ; use twbs btn coloring but not "btn" itself
                                  (update :class #(css % "btn-default")))]
                [:button btn-props [:span (str label "▾")]])
      :popover [re-com/popover-content-wrapper
                :no-clip? true
                :body [managed-popover-body route popover-id child-branch link ctx]]]]))

; props = {
;   :route    [fiddle args]
;   :tooltip  [Keyword Hiccup]
;   :popover  {:popover-id    String
;              :child-branch  String
;              :link          Ref}
;   :hidden   Boolean
;
;   ; any other standard HTML anchor props, e.g.
;   :class    String
;   :style    Map[CSS-Key, CSS-Value]
;   :on-click (e) => ()
; }
; todo all HF prop values should be monads and
; tooltip can be generated within navigate-cmp by mapping over them
(defn navigate-cmp [ctx hypercrud-props label & [class]]
  ; why doesn't this just take a link? because this needs to work with just routes
  ; props (links/build-link-props link ctx)
  ; and because nested router. Is that even needed now?
  (if-not (:hidden hypercrud-props)
    (let [hypercrud-props (update hypercrud-props :class #(css % class "hf-auto-nav"))]
      (if (some-> hypercrud-props :popover :popover-id (show-popover? ctx) deref)

        ; this means popover AND popover-showing - so omit the formula tooltip
        [popover-cmp hypercrud-props label ctx]

        ; no popover showing - so can draw tooltip
        [tooltip
         (let [tooltip-config (:tooltip hypercrud-props)
               [status label] (if (string? tooltip-config)
                                [:info tooltip-config]
                                [(first tooltip-config) (second tooltip-config)])]
           {:status status :label label})
         (if (:popover hypercrud-props)
           [popover-cmp hypercrud-props label ctx]
           [anchor-cmp (r/partial runtime/encode-route (:peer ctx)) hypercrud-props label])]))))
