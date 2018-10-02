(ns hyperfiddle.ide.fiddles.fiddle-links.renderer
  (:require
    [cats.core :refer [mlet return]]
    [contrib.ct :refer [unwrap]]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hyperfiddle.ide.console-links :refer [system-link?]]
    [hyperfiddle.ui :refer [hyper-control field table link]]))


(defn link-fiddle [val ctx {:keys [:embed-mode] :as props}]
  (fragment
    [hyper-control val ctx (dissoc props :embed-mode)]
    (when-not embed-mode
      (link :hf/affix :fiddle ctx "affix" {:disabled (system-link? @(r/fmap :db/id (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))}))))

(defn- inject-links [topnav-links fiddle]
  (assoc fiddle :fiddle/links topnav-links))

(defn- target-ide-route [ctx]
  (hyperfiddle.ide/ide-route (context/target-route ctx) ctx))

(defn- remove-children [field]
  (dissoc field :hypercrud.browser.field/children))

(defn- inject-ide-links+ [ctx]
  (let [ctx (update ctx :hypercrud.browser/field #(r/fmap remove-children %))]
    ; Hacks because hf-live is not yet modeled in the fiddle-graph, we hardcode knowledge of the IDE fiddle-graph instead
    (mlet [req (base/meta-request-for-fiddle
                 (assoc ctx
                   :hypercrud.browser/route (r/track target-ide-route ctx)
                   :branch nil))
           src-fiddle (base/hydrate-fiddle (r/track identity req) ctx)]
      (return
        (update ctx :hypercrud.browser/fiddle (partial r/fmap (r/partial inject-links (:fiddle/links src-fiddle))))))))

(let [empty-renderer (fn [val ctx props]
                       (link :hf/remove :link ctx "remove" {:disabled (system-link? @(r/fmap :db/id (:hypercrud.browser/data ctx)))}))]
  (defn renderer [val ctx {:keys [:embed-mode] :as props}]
    (let [ctx (if embed-mode
                (->> (inject-ide-links+ ctx) (unwrap (constantly nil)))
                ctx)]
      [:div
       (when-not embed-mode                                 ; more work to be done to get this popover hydrating
         (link :hf/affix :link ctx "affix"))
       [table
        (fn [ctx]
          [(field [:link/rel] ctx hyper-control)
           (field [:link/class] ctx hyper-control)
           (field [:link/fiddle] ctx link-fiddle {:embed-mode embed-mode
                                                  :options "fiddle-options"
                                                  :option-label (r/comp pr-str :fiddle/ident first)})
           (field [:link/path] ctx hyper-control)
           (when-not embed-mode (field [:link/formula] ctx hyper-control)) ; this is kind of deprecated, so hiding in hf-live to save space
           (when-not embed-mode (field [:link/tx-fn] ctx hyper-control))
           (when-not embed-mode (field [] ctx empty-renderer))])
        ctx
        (dissoc props :embed-mode)]])))
