(ns hyperfiddle.appval.domain.app
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [hypercrud.browser.core :as browser]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.core :as hc]
            [hypercrud.compile.reader :as reader]
            [hypercrud.types.ThinEntity :refer [->ThinEntity]]
            [hypercrud.util.non-fatal :refer [try-either]]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.util.string :as hc-string]
            [hyperfiddle.appval.domain.core :as hf]))


(defn process-domain [domain]
  (-> (into {} domain)
      (update :domain/code-databases
              (fn [repos]
                (->> repos
                     (map (fn [repo]
                            (-> (into {} repo)
                                ; todo this can throw
                                (update :repository/environment reader/read-string)))))))))

(defn update-hf-domain [hf-domain target-domain code-database]
  (let [target-source-uri (->> (:domain/code-databases target-domain)
                               (filter #(= (:dbhole/name %) code-database))
                               first
                               :dbhole/uri)]
    (update hf-domain :domain/code-databases
            (fn [repos]
              (->> repos
                   (map (fn [repo]
                          (if (= "root" (:dbhole/name repo))
                            (assoc-in repo [:repository/environment "$"] target-source-uri)
                            repo)))
                   set)))))

(let [always-user (atom :user)]
  (defn hf-context [ctx hf-domain target-domain target-route user-profile]
    (let [hf-domain (process-domain hf-domain)]
      (assoc ctx
        :debug "hf"
        :display-mode always-user
        :domain (update-hf-domain hf-domain target-domain (or (:code-database (:foo ctx)) ; you aren't crazy, this is weird af
                                                              (:code-database target-route)))
        :foo {:type "ide"
              :code-database (:code-database target-route)}
        :target-domain target-domain
        :target-route target-route
        :user-profile user-profile))))

(let [dispatch!-factory (fn [dispatch! action]
                          ; todo filter available actions
                          (dispatch! action)
                          nil)]
  (defn target-context [ctx target-domain target-route user-profile]
    (let [processed-domain (process-domain target-domain)]
      (assoc ctx
        :debug "target"
        :dispatch! (reactive/partial dispatch!-factory (:dispatch! ctx))
        :display-mode (reactive/cursor (.-state-atom (:peer ctx)) [:display-mode])
        :domain processed-domain
        :foo {:type "user"}
        ; repository is needed for transact! in topnav
        :repository (->> (:domain/code-databases processed-domain)
                         (filter #(= (:dbhole/name %) (:code-database target-route)))
                         first
                         (into {}))
        :target-domain target-domain
        :target-route (if (and (= "root" (:code-database target-route)) ; todo "root" is a valid user string, check uri
                               (#{17592186060855 [:db/ident :hyperfiddle/main]} (:link-id target-route)))
                        ; hack to prevent infinite loop
                        {:code-database "root"
                         :link-id 17592186045564
                         :request-params {:entity (->ThinEntity "$domains" 17592186045506)}}
                        target-route)
        :user-profile user-profile))))

(defn main-route [ctx]
  {:code-database "root"
   :link-id :hyperfiddle/main
   :entity #entity["$" (get-in ctx [:target-route :link-id])]})

(defn- ->decoded-route [maybe-decoded-route target-domain]
  (or maybe-decoded-route
      (-> (hc-string/safe-read-edn-string (:domain/home-route target-domain))
          (either/branch (constantly nil) identity))))

(defn- with-ide-ctx [maybe-decoded-route state-value ctx f]
  (let [target-domain-request (let [hf-domain-name (hf/hostname->hf-domain-name (:hostname ctx) (:hyperfiddle-hostname ctx))]
                                (hf/domain-request hf-domain-name (:peer ctx)))
        hf-domain-request (hf/domain-request "hyperfiddle" (:peer ctx))]
    (concat
      [target-domain-request hf-domain-request]
      (-> (cats/sequence [@(hc/hydrate (:peer ctx) target-domain-request)
                          @(hc/hydrate (:peer ctx) hf-domain-request)])
          (either/branch
            (constantly nil)
            (fn [[target-domain hf-domain]]
              (let [decoded-route (->decoded-route maybe-decoded-route target-domain)
                    ctx (-> (hf-context ctx hf-domain target-domain decoded-route (:user-profile state-value))
                            (update :debug str "-r"))]
                (f ctx))))))))

(defn- decoded-route->user-request [maybe-decoded-route state-value ctx]
  (let [target-domain-request (let [hf-domain-name (hf/hostname->hf-domain-name (:hostname ctx) (:hyperfiddle-hostname ctx))]
                                (hf/domain-request hf-domain-name (:peer ctx)))]
    (concat
      [target-domain-request]
      (-> @(hc/hydrate (:peer ctx) target-domain-request)
          (either/branch
            (constantly nil)
            (fn [target-domain]
              (when-let [decoded-route (->decoded-route maybe-decoded-route target-domain)]
                (let [ctx (-> (target-context ctx target-domain decoded-route (:user-profile state-value))
                              (update :debug str "-r"))]
                  (browser/request-from-route (:target-route ctx) ctx)))))))))

(defn- with-decoded-route [state-value f]
  (either/branch
    (if-let [state-route (:encoded-route state-value)]
      (try-either (routing/decode state-route))
      (either/right nil))
    (constantly nil)
    f))

(defn ide-request [state-value ctx]
  (with-decoded-route
    state-value
    (fn [maybe-decoded-route]
      (with-ide-ctx maybe-decoded-route state-value ctx
                    (fn [ctx]
                      (browser/request-from-route (:target-route ctx) ctx))))))

(defn user-request [state-value ctx]
  (with-decoded-route
    state-value
    (fn [maybe-decoded-route]
      (decoded-route->user-request maybe-decoded-route state-value ctx))))

(defn page-request [state-value ctx]
  (with-decoded-route
    state-value
    (fn [maybe-decoded-route]
      (concat
        (with-ide-ctx maybe-decoded-route state-value ctx
                      (fn [ctx]
                        (browser/request-from-route (main-route ctx) ctx)))
        (decoded-route->user-request maybe-decoded-route state-value ctx)))))
