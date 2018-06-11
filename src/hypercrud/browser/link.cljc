(ns hypercrud.browser.link
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [contrib.data :refer [unwrap]]
            [contrib.eval :as eval]
            [contrib.reactive :as r]
            [contrib.pprint :refer [pprint-str]]
            [contrib.string :refer [memoized-safe-read-edn-string]]
            [contrib.try :refer [try-either try-promise]]
            [cuerdas.core :as string]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.popovers :as popovers]
            [hypercrud.browser.q-util :as q-util]
            [hypercrud.browser.routing :as routing]
            [hyperfiddle.actions :as actions]
            [hyperfiddle.runtime :as runtime]
            [promesa.core :as p]
            [taoensso.timbre :as timbre]))


(defn popover-link? [link]
  (:link/managed? link))

(defn same-path-as? [path]
  (fn [link]
    (either/branch
      (memoized-safe-read-edn-string (str "[" (:link/path link) "]"))
      (fn [e]
        (timbre/error e)                                    ; swallow the error
        false)
      #(= path %))))

(defn options-link? [link]
  ; don't care if its inline or not, just do the right thing.
  (= :options (:link/rel link)))

(def options-processor (partial remove options-link?))

(defn rel->link [rel path ctx]
  (->> @(:hypercrud.browser/links ctx)
       (filter (same-path-as? path))
       (filter #(= (:link/rel %) rel))
       first))

(defn options-link [ctx]
  ; Needs to work with longer paths
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    (rel->link :options path ctx)))

; todo belongs in routing ns
; this is same business logic as base/request-for-link
; this is currently making assumptions on dbholes
(defn validated-route' [fiddle route ctx]
  ; We specifically hydrate this deep just so we can validate anchors like this.
  (let [[_ [$1 :as params]] route]
    (case (:fiddle/type fiddle)
      ; todo check fe conn
      ; todo merge in dbhole lookup, see: hypercrud.browser.base/request-for-link
      :query (let [q (unwrap (q-util/safe-parse-query-validated fiddle))]
               (base/validate-query-params q params ctx))
      :entity (if (not= nil $1)                             ; handles `e` but no logic for `[e a]`
                ; todo check fe conn
                (either/right route)
                (either/left {:message "malformed entity param" :data {:params params}}))
      ; nil means :blank
      (either/right route))))

(let [memoized-eval-props (memoize eval/safe-eval-string)]
  (defn eval-hc-props [props-str ctx]
    (if (and (string? props-str) (not (string/blank? props-str)))
      (cats/bind (memoized-eval-props props-str)
                 (fn [f-or-v]
                   (if (fn? f-or-v)
                     (try-either (f-or-v ctx))
                     (either/right f-or-v))))
      (either/right nil))))

(defn ^:export build-link-props-raw [unvalidated-route' link ctx] ; ctx is for display-mode
  ; this is a fine place to eval, put error message in the tooltip prop
  ; each prop might have special rules about his default, for example :visible is default true, does this get handled here?
  (let [fiddle (:link/fiddle link)                          ; can be nil - in which case route is invalid
        [_ args :as route] (unwrap unvalidated-route')
        validated-route' (validated-route' fiddle route ctx)
        user-props' (eval-hc-props (:hypercrud/props link) ctx)
        user-props (unwrap user-props')
        errors (->> [user-props' unvalidated-route' validated-route']
                    (filter either/left?) (map cats/extract) (into #{}))]
    (merge
      user-props                                            ; e.g. disabled, tooltip, style, class - anything, it gets passed to a renderer maybe user renderer
      ; doesn't handle tx-fn - meant for the self-link. Weird and prob bad.
      {:route (unwrap unvalidated-route')
       :tooltip (if-not (empty? errors)
                  [:warning (pprint-str errors)]
                  (if (:ide-active ctx)
                    [nil (pr-str args)]
                    (:tooltip user-props)))
       :class (->> [(:class user-props)
                    (if-not (empty? errors) "invalid")]
                   (remove nil?)
                   (interpose " ")
                   (apply str))})))

(let [safe-eval-string #(try-promise (eval/eval-string %))
      memoized-eval-string (memoize safe-eval-string)]
  (defn stage! [link route popover-id child-branch ctx]
    (-> (let [tx-fn-str (:link/tx-fn link)]
          (if (and (string? tx-fn-str) (not (string/blank? tx-fn-str)))
            (memoized-eval-string tx-fn-str)
            (p/resolved (constantly nil))))
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
                                     (actions/stage-popover (:peer ctx) (:hypercrud.browser/invert-route ctx) child-branch swap-fn
                                                            (actions/close-popover (:branch ctx) popover-id))))))))
        ; todo something better with these exceptions (could be user error)
        (p/catch (fn [err]
                   #?(:clj  (throw err)
                      :cljs (js/alert (pprint-str err))))))))

(defn close! [popover-id ctx]
  (runtime/dispatch! (:peer ctx) (actions/close-popover (:branch ctx) popover-id)))

(defn cancel! [popover-id child-branch ctx]
  (runtime/dispatch! (:peer ctx) (actions/batch
                                   (actions/close-popover (:branch ctx) popover-id)
                                   (actions/discard-partition child-branch))))

#?(:cljs
   (defn managed-popover-body [link route popover-id child-branch dont-branch? close! cancel! ctx]
     [:div.hyperfiddle-popover-body
      ; NOTE: this ctx logic and structure is the same as the popover branch of browser-request/recurse-request
      (let [ctx (if dont-branch? ctx (assoc ctx :branch child-branch))]
        [hypercrud.browser.core/ui-from-route route ctx])   ; cycle
      (when-not dont-branch?
        [:button {:on-click (r/partial stage! link route popover-id child-branch ctx)} "stage"])
      (if dont-branch?
        [:button {:on-click close!} "close"]
        [:button {:on-click cancel!} "cancel"])]))

(defn open! [route popover-id child-branch dont-branch? ctx]
  (runtime/dispatch! (:peer ctx)
                     (if dont-branch?
                       (actions/open-popover (:branch ctx) popover-id)
                       (actions/add-partition (:peer ctx) route child-branch (::runtime/branch-aux ctx)
                                              (actions/open-popover (:branch ctx) popover-id)))))

; if this is driven by link, and not route, it needs memoized.
; the route is a fn of the formulas and the formulas can have effects
; which have to be run only once.
(defn build-link-props [link ctx & [dont-branch?]]          ; this 'dont-branch?' argument is a holdover for topnav until 'iframe/button/anchor'
  ; Draw as much as possible even in the presence of errors, still draw the link, collect all errors in a tooltip.
  ; Error states:
  ; - no route
  ; - invalid route
  ; - broken user formula
  ; - broken user txfn
  ; - broken user visible fn
  ; If these fns are ommitted (nil), its not an error.
  (let [route' (routing/build-route' link ctx)
        hypercrud-props (build-link-props-raw route' link ctx)
        popover-props (if (popover-link? link)
                        (if-let [route (and (:link/managed? link) (either/right? route') (cats/extract route'))]
                          ; If no route, there's nothing to draw, and the anchor tooltip shows the error.
                          (let [popover-id (popovers/popover-id link ctx)
                                child-branch (popovers/branch ctx link)
                                open! (r/partial open! route popover-id child-branch dont-branch? ctx)
                                close! (r/partial close! popover-id ctx)
                                cancel! (r/partial cancel! popover-id child-branch ctx)]
                            {:showing? (runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :popovers popover-id])
                             :body #?(:cljs [managed-popover-body link route popover-id child-branch dont-branch? close! cancel! ctx]
                                      :clj  nil)
                             :open! open!
                             :close! (if dont-branch? close! cancel!)})))]
    (merge hypercrud-props {:popover popover-props})))
