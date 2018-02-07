(ns hypercrud.browser.link
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.popovers :as popovers]
            [hypercrud.browser.q-util :as q-util]
            [hypercrud.browser.routing :as routing]
            [hypercrud.compile.eval :as eval]
            [hypercrud.util.core :refer [pprint-str unwrap]]
            [hypercrud.util.non-fatal :refer [try-either]]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.util.string :as hc-string]
            [hyperfiddle.foundation.actions :as foundation-actions]
            [promesa.core :as p]
            [taoensso.timbre :as timbre]
            [hypercrud.browser.base :as base]))


(defn option-link? [link]
  ; don't care if its inline or not, just do the right thing.
  (= :options (:link/rel link)))

(defn popover-link? [link]
  (:link/managed? link))

(def build-pathed-links-lookup
  (memoize                                                  ; memory leak; should back by local ratom
    (fn [anchors]
      (reduce (fn [acc anchor]
                (-> (hc-string/memoized-safe-read-edn-string (str "[" (:link/path anchor) "]"))
                    (either/branch
                      (fn [e]
                        (timbre/error e)
                        ; swallow the error
                        acc)
                      (fn [path]
                        (update-in acc (conj path :links) conj anchor)))))
              {}
              anchors))))

(defn process-popover-link [link]
  (if (popover-link? link)
    (assoc link :link/render-inline? false)
    link))

(defn process-option-links [links ctx]
  (let [[options-link] (filter option-link? links)
        links (remove option-link? links)]
    [links options-link]))

(defn links-lookup [links path]                             ; the real one used by request side
  (-> (build-pathed-links-lookup links)
      (get-in (conj path :links))))

(defn links-lookup' [links path]                            ; cosmetic UI side because popover hacks. If popovers hydrate seperate, this goes away
  (->> (links-lookup links path)
       ; the cosmetic change
       (map process-popover-link)))

; todo belongs in routing ns
; this is same business logic as base/request-for-link
; this is currently making assumptions on dbholes
(defn validated-route' [fiddle route ctx]
  ; We specifically hydrate this deep just so we can validate anchors like this.
  (let [params (:request-params route)]
    (case (:fiddle/type fiddle)
      ; todo check fe conn
      ; todo merge in dbhole lookup, see: hypercrud.browser.base/request-for-link
      :query (let [q (unwrap (q-util/safe-parse-query-validated fiddle))]
               (base/validate-query-params q params ctx))
      :entity (if (not= nil params)                         ; handles `e` but no logic for `[e a]`
                ; todo check fe conn
                (either/right route)
                (either/left {:message "malformed entity param" :data {:params params}}))
      :blank (either/right route)
      (either/left {:message "route has no fiddle" :data {:route route}}))))

(defn get-or-apply' [expr & args]
  (if (fn? expr)
    (try-either (apply expr args))
    (either/right expr)))

(defn ^:export build-link-props-raw [unvalidated-route' link ctx] ; ctx is for display-mode

  ; this is a fine place to eval, put error message in the tooltip prop
  ; each prop might have special rules about his default, for example :visible is default true, does this get handled here?

  (let [fiddle (:link/fiddle link)                          ; can be nil - in which case route is invalid
        route (unwrap unvalidated-route')
        validated-route' (validated-route' fiddle route ctx)
        user-props' (cats/bind (eval/eval-str (:hypercrud/props link))
                               (fn [user-expr]
                                 (if user-expr
                                   (get-or-apply' user-expr ctx)
                                   (either/right nil))))
        user-props-map-raw (cats/extract (cats/mplus user-props' (either/right nil)))
        user-prop-val's (map #(get-or-apply' % ctx) (vals user-props-map-raw))
        user-prop-vals (map #(cats/extract (cats/mplus % (either/right nil))) user-prop-val's)
        errors (->> (concat [user-props' unvalidated-route' validated-route'] user-prop-val's)
                    (filter either/left?) (map cats/extract) (into #{}))
        user-props (zipmap (keys user-props-map-raw) user-prop-vals)]
    (merge
      user-props                                            ; e.g. disabled, tooltip, style, class - anything, it gets passed to a renderer maybe user renderer
      ; doesn't handle tx-fn - meant for the self-link. Weird and prob bad.
      {:route (-> unvalidated-route' (cats/mplus (either/right nil)) (cats/extract))
       :tooltip (if-not (empty? errors)
                  [:warning (pprint-str errors)]
                  (if (:ide-active ctx)
                    [nil (pr-str (dissoc route :code-database :fiddle-id))]
                    (:tooltip user-props)))
       :class (->> [(:class user-props)
                    (if-not (empty? errors) "invalid")]
                   (remove nil?)
                   (interpose " ")
                   (apply str))})))

(defn stage! [link route popover-id ctx]
  (let [user-txfn (or (unwrap (eval/eval-str (:link/tx-fn link))) (constantly nil))]
    (-> (p/promise
          (fn [resolve! reject!]
            (let [swap-fn (fn [multi-color-tx]
                            ; todo why does the user-txfn have access to the parent fiddle's context
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
              ((:dispatch! ctx) (foundation-actions/stage-popover (:peer ctx) popover-id (:branch ctx) swap-fn)))))
        ; todo something better with these exceptions (could be user error)
        (p/catch (fn [err]
                   #?(:clj  (throw err)
                      :cljs (js/alert (pprint-str err))))))))

(defn close! [popover-id ctx]
  ((:dispatch! ctx) (foundation-actions/close-popover popover-id)))

(defn cancel! [popover-id ctx]
  ((:dispatch! ctx) (foundation-actions/discard-branched-popover popover-id (:branch ctx))))

(defn managed-popover-body [link route popover-id dont-branch? ctx]
  [:div.hyperfiddle-popover-body
   ; NOTE: this ctx logic and structure is the same as the popover branch of browser-request/recurse-request
   (let [ctx (-> ctx
                 (context/clean)
                 (update :hypercrud.browser/debug #(str % ">popover-link[" (:db/id link) ":" (or (:link/rel link) (:anchor/prompt link)) "]")))]
     #?(:clj  (assert false "todo")
        :cljs [hypercrud.browser.core/ui-from-route route ctx])) ; cycle
   (when-not dont-branch?
     [:button {:on-click (reactive/partial stage! link route popover-id ctx)} "stage"])
   ; TODO also cancel on escape
   (if dont-branch?
     [:button {:on-click (reactive/partial close! popover-id ctx)} "close"]
     [:button {:on-click (reactive/partial cancel! popover-id ctx)} "cancel"])])

(defn open! [route popover-id dont-branch? ctx]
  ((:dispatch! ctx)
    (if dont-branch?
      (foundation-actions/open-popover popover-id)
      (foundation-actions/open-branched-popover (:peer ctx) popover-id (:branch ctx) route))))

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
                                ctx (if dont-branch? ctx (context/anchor-branch ctx link))]
                            {:showing? (reactive/cursor (-> ctx :peer .-state-atom) [:popovers popover-id])
                             :body [managed-popover-body link route popover-id dont-branch? ctx]
                             :open! (reactive/partial open! route popover-id dont-branch? ctx)})))]
    (merge hypercrud-props {:popover popover-props})))
