(ns hypercrud.browser.anchor
  (:require [cljs.reader :as reader]
            [clojure.set :as set]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx]
            [hypercrud.compile.eval :refer [eval-str]]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.types :refer [->DbId]]
            [hypercrud.util :as util]
            [promesa.core :as p]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.browser.link-util :as link-util]
            [reagent.core :as r]))


(defn build-anchor-route
  ([domain project link-dbid formula-str branch param-ctx]
    ;(assert project)                                         ; safe - maybe constructing it now
   {:domain domain
    :project project
    :link-dbid link-dbid #_:id
    :branch (or (some-> (:db param-ctx) .-branch (str "`" branch)) branch)
    :query-params (try                                      ; todo return monad
                    (-> (eval-str formula-str)
                        (q-util/run-formula! param-ctx))
                    (catch js/Error e {}))})
  ([link formula-str branch param-ctx]
   (build-anchor-route
     (-> link :hypercrud/owner :database/domain)
     (-> link :hypercrud/owner :database/ident)
     (-> link :db/id)
     formula-str
     branch
     param-ctx))
  ([anchor param-ctx]
   (build-anchor-route (:anchor/link anchor) (:anchor/formula anchor) (:anchor/branch anchor) param-ctx)))

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
        route (if (:anchor/link anchor) (build-anchor-route anchor param-ctx #_"not branched yet! about to branch"))
        route-props (if route (build-anchor-props-raw route (:anchor/link anchor) param-ctx))
        param-ctx (if-let [branch (:branch route)]
                    (update param-ctx :db #(hc/db (:response param-ctx) (.-conn-id %) branch))
                    param-ctx)]
    (doall
      (merge
        (if txfn
          ; do we need to hydrate any dependencies in this chain?
          {:txfn #(let [result (txfn param-ctx %)]          ; branched
                    (-> (if-not (p/promise? result) (p/resolved result) result) ; txfn may be sync or async
                        (p/branch (fn [result]
                                    ; the branch is out of date
                                    ((:discard! param-ctx) (.-conn-id (:db param-ctx)) (.-branch (:db param-ctx)))
                                    ; stage the result from biz logic into master
                                    ((:with! param-ctx) (.-conn-id (:db param-ctx)) nil (:tx result))
                                    nil)
                                  (fn error [why]
                                    (case why
                                      :cancel ((:discard! param-ctx) (.-conn-id (:db param-ctx)) (.-branch (:db param-ctx)))
                                      (js/console.error why))
                                    nil))))})

        (if (:anchor/branch anchor)                         ; the whole point of popovers is managed branches.
          {:modal-tx (hc/tx (:response param-ctx) (:db param-ctx)) ; biz logic runs on the branch stage
           :popover (fn []
                      [:div
                       (case (:display-mode param-ctx)
                         :xray [(:navigate-cmp param-ctx) route-props "self"]
                         nil)
                       [hypercrud.browser.core/safe-ui'     ; cycle
                        route                               ; draw the branch
                        (dissoc param-ctx :result :entity :attribute :value :layout)]])}
          route-props)))))

(defn anchor-visible? [anchor param-ctx]
  (let [visible-src (:anchor/visible? anchor)
        visible-fn (if-not (empty? visible-src)
                     (let [{:keys [value error]} (eval-str visible-src)]
                       (if error (js/alert (str "cljs eval error: " error)))
                       value)
                     (constantly true))]
    (visible-fn param-ctx)))
