(ns hypercrud.browser.anchor
  (:require [cats.core :as cats :refer [mlet return]]
            [cats.monad.either :as either]
            [cats.monad.exception :as exception]
            [clojure.set :as set]
            [hypercrud.browser.auto-anchor-formula :refer [auto-entity-dbid]]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.browser.context :as context]
            [hypercrud.compile.eval :as eval :refer [eval-str']]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.state.actions.core :as actions]
            [hypercrud.util.core :refer [pprint-str]]
            [hypercrud.util.monad :refer [exception->either]]
            [promesa.core :as p]
            [reagent.core :as reagent]))


(defn popover-id [anchor ctx]
  {:anchor-id (-> anchor :db/id :id)
   :branch (:branch ctx)
   :location (auto-entity-dbid ctx)})

(defn option-anchor? [anchor]
  ; don't care if its inline or not, just do the right thing.
  (= :options (:anchor/ident anchor)))

(defn popover-anchor? [anchor]
  (:anchor/managed? anchor))

(defn safe-run-user-code-str' [code-str & args]
  (if-let [code-str (eval/validate-user-code-str code-str)]
    (mlet [user-fn (eval-str' code-str)]
      (if user-fn
        (-> (exception/try-on (apply user-fn args))
            exception->either)
        (return nil)))
    (either/right nil)))

(defn ^:export build-anchor-route' [anchor param-ctx]
  (mlet [query-params (if-let [code-str (:anchor/formula anchor)]
                        (safe-run-user-code-str' code-str param-ctx)
                        (either/right nil))
         link-dbid (if-let [page (:anchor/link anchor)]
                     (either/right (:db/id page))
                     (either/left {:message "anchor has no link" :data {:anchor anchor}}))]
    (return {:domain (-> anchor :anchor/link :hypercrud/owner :database/domain)
             :project (-> anchor :anchor/link :hypercrud/owner :database/ident)
             :link-dbid link-dbid
             :query-params query-params})))

; this is same business logic as base/request-for-link
; this is currently making assumptions on dbholes
(defn validated-route' [link route]
  ; We specifically hydrate this deep just so we can validate anchors like this.
  (let [have (set (keys (into {} (remove (comp nil? val) (:query-params route)))))]
    (case (:request/type link)
      :query (let [q (-> (q-util/safe-parse-query-validated link)
                         (cats/mplus (either/right []))
                         (cats/extract))
                   ; todo check fe conn
                   ; todo merge in dbhole lookup, see: hypercrud.browser.base/request-for-link
                   need (set (q-util/parse-param-holes q))
                   missing (set/difference need have)]
               (if (empty? missing)
                 (either/right route)
                 (either/left {:message "missing query params" :data {:have have :missing missing}})))
      :entity (if (not= nil (-> route :query-params :entity)) ; add logic for a
                ; todo check fe conn
                (either/right route)
                (either/left {:message "missing query params" :data {:have have :missing #{:entity}}}))
      :blank (either/right route)
      (either/left {:message "route has no link" :data {:route route}})
      #_(either/right route) #_"wtf???  \"{:hypercrud/owner {:database/domain nil, :database/ident \"hyperfiddle\"}, :db/id #DbId[[:link/ident :hyperfiddle/new-page-popover] 17592186045422]}\"   ")))

(defn get-or-apply' [expr & args]
  (if (fn? expr)
    (exception->either (exception/try-on (apply expr args)))
    (either/right expr)))

(defn build-anchor-props-raw [unvalidated-route' anchor param-ctx] ; param-ctx is for display-mode

  ; this is a fine place to eval, put error message in the tooltip prop
  ; each prop might have special rules about his default, for example :visible is default true, does this get handled here?

  (let [link (:anchor/link anchor)                          ; can be nil - in which case route is invalid
        route (-> unvalidated-route' (cats/mplus (either/right nil)) (cats/extract))
        validated-route' (validated-route' link route)
        user-props' (if-let [user-code-str (eval/validate-user-code-str (:hypercrud/props anchor))]
                      (mlet [user-expr (eval-str' user-code-str)]
                        (get-or-apply' user-expr param-ctx))
                      (either/right nil))
        user-props-map-raw (cats/extract (cats/mplus user-props' (either/right nil)))
        user-prop-val's (map #(get-or-apply' % param-ctx) (vals user-props-map-raw))
        user-prop-vals (map #(cats/extract (cats/mplus % (either/right nil))) user-prop-val's)
        errors (->> (concat [user-props' unvalidated-route' validated-route'] user-prop-val's)
                    (filter either/left?) (map cats/extract) (into #{}))
        user-props (zipmap (keys user-props-map-raw) user-prop-vals)]
    (merge
      user-props                                            ; e.g. disabled, tooltip, style, class - anything, it gets passed to a renderer maybe user renderer
      ; doesn't handle tx-fn - meant for the self-link. Weird and prob bad.
      {:route (-> unvalidated-route' (cats/mplus (either/right nil)) (cats/extract))
       :style {:color (connection-color/connection-color (-> link :hypercrud/owner :db/id :id))}
       :tooltip (if-not (empty? errors)
                  [:warning (pprint-str errors)]
                  (case @(:display-mode param-ctx)
                    :xray [nil (pr-str (:query-params route))]
                    :user (:tooltip user-props)))
       :class (->> [(:class user-props)
                    (if-not (empty? errors) "invalid")]
                   (remove nil?)
                   (interpose " ")
                   (apply str))})))

(defn managed-popover-body [anchor route popover-id param-ctx]
  (let [stage! (fn []
                 (let [user-txfn (some-> (eval/validate-user-code-str (:anchor/tx-fn anchor)) eval-str' (cats/mplus (either/right nil)) (cats/extract))
                       user-txfn (or user-txfn (fn [ctx multi-color-tx modal-route] {:tx multi-color-tx}))]
                   (-> (p/promise
                         (fn [resolve! reject!]
                           (let [swap-fn (fn [multi-color-tx]
                                           ; todo why does the user-txfn have access to the parent link's context
                                           (let [result (let [result (user-txfn param-ctx multi-color-tx route)]
                                                          ; txfn may be sync or async
                                                          (if-not (p/promise? result) (p/resolved result) result))]
                                             ; let the caller of this :stage fn know the result
                                             ; This is super funky, a swap-fn should not be effecting, but seems like it would work.
                                             (p/branch result
                                                       (fn [v] (resolve! nil))
                                                       (fn [e]
                                                         (reject! e)
                                                         (js/console.warn e)))

                                             ; return the result to the action, it could be a promise
                                             result))]
                             ((:dispatch! param-ctx) (actions/stage-popover (:branch param-ctx) popover-id swap-fn)))))
                       ; todo something better with these exceptions (could be user error)
                       (p/catch #(-> % pprint-str js/alert)))))
        ; NOTE: this param-ctx logic and structure is the same as the popover branch of browser-request/recurse-request
        param-ctx (-> param-ctx
                      (context/clean)
                      (update :debug #(str % ">popover-link[" (:db/id anchor) ":" (or (:anchor/ident anchor) (:anchor/prompt anchor)) "]")))]
    [:div.managed-popover
     [hypercrud.browser.core/ui-from-route route param-ctx] ; cycle
     [:button {:on-click stage!} "stage"]]))

(defn visible? [anchor ctx]
  (-> (if-let [code-str (eval/validate-user-code-str (:anchor/visible? anchor))] ; also inline links !
        (mlet [user-fn (eval-str' code-str)]
          (-> (exception/try-on (user-fn ctx))
              exception->either))
        (either/right true))
      (cats/mplus (either/right true))
      (cats/extract)))

; if this is driven by anchor, and not route, it needs memoized.
; the route is a fn of the formulas and the formulas can have effects
; which have to be run only once.
(defn build-anchor-props [anchor param-ctx]
  ; Draw as much as possible even in the presence of errors, still draw the link, collect all errors in a tooltip.
  ; Error states:
  ; - no route
  ; - invalid route
  ; - broken user formula
  ; - broken user txfn
  ; - broken user visible fn
  ; If these fns are ommitted (nil), its not an error.
  (let [visible? (visible? anchor param-ctx)
        route' (build-anchor-route' anchor param-ctx)
        hypercrud-props (build-anchor-props-raw route' anchor param-ctx)
        popover-props (if (popover-anchor? anchor)
                        (if-let [route (and (:anchor/managed? anchor) (either/right? route') (cats/extract route'))]
                          ; If no route, there's nothing to draw, and the anchor tooltip shows the error.
                          (let [popover-id (popover-id anchor param-ctx) ;we want the context before we branch
                                param-ctx (context/anchor-branch param-ctx anchor)]
                            {:showing? (reagent/cursor (-> param-ctx :peer .-state-atom) [:popovers popover-id])
                             :body [managed-popover-body anchor route popover-id param-ctx]
                             :open! #((:dispatch! param-ctx) (actions/open-popover popover-id))
                             :cancel! #((:dispatch! param-ctx) (actions/cancel-popover (:branch param-ctx) popover-id))})))
        anchor-props-hidden {:hidden (not visible?)}]
    (merge anchor-props-hidden hypercrud-props {:popover popover-props})))
