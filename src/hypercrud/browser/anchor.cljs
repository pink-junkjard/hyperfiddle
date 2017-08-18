(ns hypercrud.browser.anchor
  (:require [cats.core :as cats :refer [mlet return]]
            [cats.monad.either :as either]
            [cats.monad.exception :as exception]
            [clojure.set :as set]
            [hypercrud.browser.auto-anchor-formula :refer [auto-entity-dbid]]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.client.core :as hc]
            [hypercrud.compile.eval :as eval :refer [eval-str']]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.state.actions.core :as actions]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.monad :refer [exception->either]]
            [promesa.core :as p]
            [hypercrud.util.core :as util :refer [pprint-str]]))


(defn safe-run-user-code-str' [code-str & args]
  (if-let [code-str (eval/validate-user-code-str code-str)]
    (mlet [user-fn (eval-str' code-str)]
      (if user-fn
        (-> (exception/try-on (apply user-fn args))
            exception->either)
        (return nil)))))

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
      :query (mlet [q (either/right (q-util/safe-parse-query-validated link))]
               (let [need (set (q-util/parse-param-holes q))
                     missing (set/difference need have)]
                 (if (empty? missing)
                   (either/right route)
                   (either/left {:message "missing query params" :data {:have have :missing missing}}))))
      :entity (if (not= nil (-> route :query-params :entity)) ; add logic for a
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
                  (case (:display-mode param-ctx)
                    :xray [nil (pr-str (:query-params route))]
                    :user (:tooltip user-props)))
       :class (->> [(:class user-props)
                    (if-not (empty? errors) "invalid")]
                   (remove nil?)
                   (interpose " ")
                   (apply str))})))

(defn anchor-branch-logic [anchor param-ctx]
  (if (:anchor/managed? anchor)
    (if-let [db (:db param-ctx)]
      (let [branch (branch/encode-branch-child (.-branch db) (:id (auto-entity-dbid param-ctx)))]
        (-> param-ctx
            ; if we are an index link, what are we forking? Provide a binding
            (assoc-in [:branches (.-conn-id db)] branch)
            (update :db #(hc/db (:peer param-ctx) (.-conn-id %) branch))))
      (do
        (js/console.warn "You are attempting to branch an index-link. We can't deduce the :db to branch, you must explicitly set it in user bindings.")
        param-ctx))
    param-ctx))

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
  (let [visible? (-> (if-let [code-str (eval/validate-user-code-str (:anchor/visible? anchor))] ; also inline links !
                       (mlet [user-fn (eval-str' code-str)]
                         (-> (exception/try-on (user-fn param-ctx))
                             exception->either))
                       (either/right true))
                     (cats/mplus (either/right true))
                     (cats/extract))
        route' (build-anchor-route' anchor param-ctx)
        anchor-props-route (build-anchor-props-raw route' anchor param-ctx)
        param-ctx (anchor-branch-logic anchor param-ctx)
        anchor-props-txfn (if-let [user-txfn (some-> (eval/validate-user-code-str (:anchor/tx-fn anchor))
                                                     eval-str'
                                                     (cats/mplus (either/right nil))
                                                     (cats/extract))]
                            (do
                              ;(assert (-contains-key? param-ctx :db))
                              ; do we need to hydrate any dependencies in this chain?
                              {:txfns {:stage (fn []
                                                (p/promise (fn [resolve reject]
                                                             (let [swap-fn (fn [tx-from-modal]
                                                                             ; todo why does the user-txfn have access to the parent link's :db :result etc?
                                                                             (let [result (let [result (user-txfn param-ctx tx-from-modal)]
                                                                                            ; txfn may be sync or async
                                                                                            (if-not (p/promise? result) (p/resolved result) result))]
                                                                               ; let the caller of this :stage fn know the result
                                                                               (p/branch result
                                                                                         (fn [result] (resolve nil))
                                                                                         (fn [why]
                                                                                           (reject why)
                                                                                           (js/console.error why)))

                                                                               ; return the result to the action
                                                                               result))]
                                                               ((:dispatch! param-ctx) (actions/stage-popover (.-conn-id (:db param-ctx)) (.-branch (:db param-ctx)) swap-fn))))))
                                       :cancel #((:dispatch! param-ctx) (actions/discard (.-conn-id (:db param-ctx)) (.-branch (:db param-ctx))))}}))

        ; the whole point of popovers is managed branches
        anchor-props-popover (if-let [route (and (:anchor/managed? anchor) (either/right? route') (cats/extract route'))]
                               ; If no route, there's nothing to draw, and the anchor tooltip shows the error.
                               {:popover (fn []
                                           [:div
                                            (case (:display-mode param-ctx)
                                              :xray [(:navigate-cmp param-ctx) anchor-props-route "self"]
                                              nil)
                                            ; NOTE: this param-ctx logic and structure is the same as the popover branch of browser-request/recurse-request
                                            [hypercrud.browser.core/safe-ui' ; cycle
                                             route          ; draw the branch
                                             (-> param-ctx
                                                 (dissoc :result :db :find-element :entity :attribute :value :layout :field)
                                                 (update :debug #(str % ">popover-link[" (:db/id anchor) ":" (:anchor/prompt anchor) "]")))]])})
        anchor-props-hidden {:hidden (not visible?)}]
    (merge anchor-props-route anchor-props-txfn anchor-props-popover anchor-props-hidden)))
