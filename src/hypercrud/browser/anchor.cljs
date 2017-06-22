(ns hypercrud.browser.anchor
  (:require [clojure.set :as set]
            [hypercrud.browser.auto-anchor-formula :refer [auto-entity-dbid]]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.browser.link-util :as link-util]
            [hypercrud.compile.eval :refer [eval-str]]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.runtime.state.actions :as actions]   ; todo bad dep
            [hypercrud.util.branch :as branch]
            [promesa.core :as p]))


(defn safe-run-user-code-str [code-str param-ctx]
  (try                                                      ; todo return monad
    (-> (eval-str code-str)
        (q-util/run-formula! param-ctx))
    (catch js/Error e {})))

(defn build-anchor-route
  ([domain project link-dbid formula-str param-ctx]
    ;(assert project)                                         ; safe - maybe constructing it now
   {:domain domain
    :project project
    :link-dbid link-dbid #_:id
    :query-params (safe-run-user-code-str formula-str param-ctx)})
  ([link formula-str param-ctx]
   (build-anchor-route
     (-> link :hypercrud/owner :database/domain)
     (-> link :hypercrud/owner :database/ident)
     (-> link :db/id)
     formula-str
     param-ctx))
  ([anchor param-ctx]
   (build-anchor-route (:anchor/link anchor) (:anchor/formula anchor) param-ctx)))

(defn holes-filled? [hole-names query-params-map]
  (set/subset? (set hole-names) (set (keys (into {} (remove (comp nil? val) query-params-map))))))

(defn anchor-valid? [link route]                            ; could return monad to say why
  ; We specifically hydrate this deep just so we can validate anchors like this.
  (case (link-util/link-type link)
    :link-query (some-> link :link/request
                        q-util/safe-parse-query-validated
                        q-util/parse-param-holes
                        (holes-filled? (:query-params route)))
    :link-entity (not= nil (-> route :query-params :entity)) ; add logic for a
    :link-blank true))

(defn anchor-valid?' [anchor route]
  (anchor-valid? (:anchor/link anchor) route))

(defn anchor-tooltip [link url-params param-ctx]
  (case (:display-mode param-ctx)
    :xray (if (anchor-valid? link url-params)
            [nil (pr-str (:query-params url-params))]
            [:warning (pr-str (:query-params url-params))])
    nil))

(defn build-anchor-props-raw [route link param-ctx]         ; param-ctx is for display-mode
  ; doesn't handle tx-fn - meant for the self-link. Weird and prob bad.
  {:route route
   :style {:color (connection-color/connection-color (-> link :hypercrud/owner :db/id :id))}
   :tooltip (anchor-tooltip link route param-ctx)
   :class (if-not (anchor-valid? link route) "invalid")})

(defn is-anchor-managed? [anchor]
  (let [{r :anchor/repeating? e :anchor/find-element a :anchor/attribute ident :anchor/ident} anchor]
    (and (:anchor/tx-fn anchor) (:anchor/link anchor))))

(defn anchor-branch-logic [anchor param-ctx]
  (if (is-anchor-managed? anchor)
    (if-let [db (:db param-ctx)]
      (let [branch (branch/encode-branch-child (second db) (:id (auto-entity-dbid param-ctx)))]
        (-> param-ctx
            ; if we are an index link, what are we forking? Provide a binding
            (assoc-in [:branches (first db)] branch)
            (assoc :db [(first db) branch])))
      ; Inform user via tooltip that we can't branch an index link because there is no db in scope. Explicitly set db in user bindings.
      param-ctx)
    param-ctx))

; if this is driven by anchor, and not route, it needs memoized.
; the route is a fn of the formulas and the formulas can have effects
; which have to be run only once.
(defn build-anchor-props [anchor param-ctx]
  (let [param-ctx (assoc param-ctx :link-owner (-> anchor :anchor/link :hypercrud/owner)) ; txfn may need this
        txfn (if-let [txfn-str (:anchor/tx-fn anchor)]
               (let [{value :value error :error} (eval-str txfn-str)]
                 ; non-fatal error, report it here so user can fix it
                 (if error (js/alert (str "cljs eval error: " error))) ; return monad so tooltip can draw the error
                 value))

        ; these three things can fail and need to inform user error
        route (if (:anchor/link anchor) (build-anchor-route anchor param-ctx #_"links & routes have nothing to do with branches"))
        route-props (if route (build-anchor-props-raw route (:anchor/link anchor) param-ctx))
        param-ctx (anchor-branch-logic anchor param-ctx)]
    (doall
      (merge
        (if txfn
          ; do we need to hydrate any dependencies in this chain?
          {:txfns {:stage (fn []
                            ; this is a roundabout way of just swapping tx-from-modal with a thunk
                            ; this whole :stage fn needs a rewrite from scratch
                            (p/branch (let [tx-from-modal (assert false "todo") #_(hc/tx hc/*peer* (:db param-ctx))
                                            result (txfn param-ctx tx-from-modal)]
                                        (if-not (p/promise? result) (p/resolved result) result)) ; txfn may be sync or async
                                      (fn [result]
                                        (let [[conn-id branch] (:db param-ctx)]
                                          ((:dispatch! param-ctx) (actions/stage-popover conn-id branch (:tx result) (:app-route result))))
                                        nil)
                                      (fn [why]
                                        (js/console.error why))))
                   :cancel #((:dispatch! param-ctx) (apply actions/discard (:db param-ctx)))}})

        (if (is-anchor-managed? anchor)                     ; the whole point of popovers is managed branches.
          {:popover (fn []
                      [:div
                       (case (:display-mode param-ctx)
                         :xray [(:navigate-cmp param-ctx) route-props "self"]
                         nil)
                       [hypercrud.browser.core/safe-ui'     ; cycle
                        route                               ; draw the branch
                        (dissoc param-ctx :result :db :find-element :entity :attribute :value :layout)]])})
        route-props))))

(defn anchor-visible? [anchor param-ctx]
  (let [visible-src (:anchor/visible? anchor)
        visible-fn (if-not (empty? visible-src)
                     (let [{:keys [value error]} (eval-str visible-src)]
                       (if error (js/alert (str "cljs eval error: " error)))
                       value)
                     (constantly true))]
    (visible-fn param-ctx)))
