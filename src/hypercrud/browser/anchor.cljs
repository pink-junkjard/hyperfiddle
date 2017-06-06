(ns hypercrud.browser.anchor
  (:require [clojure.set :as set]
            [hypercrud.browser.auto-anchor-branch :refer [auto-branch]]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.browser.link-util :as link-util]
            [hypercrud.client.core :as hc]
            [hypercrud.compile.eval :refer [eval-str]]
            [hypercrud.form.q-util :as q-util]
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
    (or
      (and (not r) a)
      (and (not r) e (not a)))))

(defn anchor-branch-logic [anchor param-ctx]
  (if (is-anchor-managed? anchor)
    (let [branch (branch/encode-branch-child (some-> (:db param-ctx) .-branch)
                                             (safe-run-user-code-str (auto-branch anchor) param-ctx))]
      (-> param-ctx
          (assoc-in [:branches (.-conn-id (:db param-ctx))] branch)
          (update :db #(hc/db (:response param-ctx) (.-conn-id %) branch))))
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
        route (if (:anchor/link anchor) (build-anchor-route anchor param-ctx #_"links & routes have nothing to do with branches"))
        route-props (if route (build-anchor-props-raw route (:anchor/link anchor) param-ctx))
        param-ctx (anchor-branch-logic anchor param-ctx)]
    (doall
      (merge
        (if txfn
          (let [tx-from-modal (hc/tx (:response param-ctx) (:db param-ctx))]
            ; do we need to hydrate any dependencies in this chain?
            {:txfns {:stage (fn []
                              (p/branch (let [result (txfn param-ctx tx-from-modal)]
                                          (if-not (p/promise? result) (p/resolved result) result)) ; txfn may be sync or async)
                                        (fn [result]
                                          ; the branch is out of date
                                          ((:discard! param-ctx) (.-conn-id (:db param-ctx)) (.-branch (:db param-ctx)))
                                          ; stage the result from biz logic into master
                                          ((:with! param-ctx) (.-conn-id (:db param-ctx)) nil (:tx result))
                                          nil)
                                        (fn [why]
                                          (js/console.error why))))
                     :cancel #((:discard! param-ctx) (.-conn-id (:db param-ctx)) (.-branch (:db param-ctx)))}}))

        (if (is-anchor-managed? anchor)                     ; the whole point of popovers is managed branches.
          {:popover (fn []
                      [:div
                       (case (:display-mode param-ctx)
                         :xray [(:navigate-cmp param-ctx) route-props "self"]
                         nil)
                       [hypercrud.browser.core/safe-ui'     ; cycle
                        route                               ; draw the branch
                        (dissoc param-ctx :result :entity :attribute :value :layout)]])})
        route-props))))

(defn anchor-visible? [anchor param-ctx]
  (let [visible-src (:anchor/visible? anchor)
        visible-fn (if-not (empty? visible-src)
                     (let [{:keys [value error]} (eval-str visible-src)]
                       (if error (js/alert (str "cljs eval error: " error)))
                       value)
                     (constantly true))]
    (visible-fn param-ctx)))
