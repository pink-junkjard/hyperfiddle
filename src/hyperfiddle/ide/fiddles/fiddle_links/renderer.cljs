(ns hyperfiddle.ide.fiddles.fiddle-links.renderer
  (:require
    [cats.core :refer [mlet return]]
    [cats.monad.either :as either]
    [contrib.ct :refer [unwrap]]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [hypercrud.browser.base]
    [hypercrud.browser.context :as context]
    [hypercrud.client.core :as hc]
    [hyperfiddle.ide.console-links :refer [system-link?]]
    [hyperfiddle.ui :refer [hyper-control field table link]]
    [hyperfiddle.ui.select$ :refer [select-error-cmp]]))


(def editable-if-shadowed?
  #{:link/class :link/fiddle :link/formula :link/tx-fn})

(defn read-only? [ctx]
  (if (:hypercrud.browser/data ctx)                         ; be robust to being called on labels
    (let [entity (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])
          sys? (system-link? @(r/fmap :db/id entity))
          shadow? @(r/fmap :hypercrud/sys? entity)]
      (or sys? (and shadow? (not (editable-if-shadowed? (last (:hypercrud.browser/path ctx)))))))))

(defn read-only-cell [val ctx props]
  ; Need to delay until we have the value ctx to compute this, which means its a value renderer not a field prop
  (let [props (assoc props :read-only (read-only? ctx))]
    [hyper-control val ctx props]))

(defn link-fiddle [val ctx props]
  (fragment
    [hyper-control val ctx (assoc props :read-only (read-only? ctx))]
    (link :hf/affix :fiddle ctx "affix" {:disabled (system-link? @(r/fmap :db/id (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))})))

(defn- inject-links [topnav-links fiddle]
  (assoc fiddle :fiddle/links topnav-links))

(defn- target-ide-route [ctx]
  (hyperfiddle.ide/ide-fiddle-route (context/target-route ctx) ctx))

(defn remove-children [field]
  (dissoc field :hypercrud.browser.field/children))

(defn inject-topnav-links+ [ctx]
  (let [ctx (update ctx :hypercrud.browser/field #(r/fmap remove-children %))]
    ; Hacks because hf-live is not yet modeled in the fiddle-graph, we hardcode knowledge of the IDE fiddle-graph instead
    (-> (mlet [req (hypercrud.browser.base/meta-request-for-fiddle
                     (assoc ctx
                       :hypercrud.browser/route (r/track target-ide-route ctx)
                       :branch nil))
               topnav-fiddle (hypercrud.browser.base/hydrate-fiddle (r/track identity req) ctx)]
          (return
            (update ctx :hypercrud.browser/fiddle (partial r/fmap (r/partial inject-links (:fiddle/links topnav-fiddle))))))
        (->> (unwrap (constantly nil))))))

(let [empty-renderer (fn [val ctx props]
                       (link :hf/remove :link ctx "remove" {:disabled (system-link? @(r/fmap :db/id (:hypercrud.browser/data ctx)))}))]
  (defn renderer [val ctx {:keys [:embed-mode] :as props}]
    [table
     #_(partial form (fn [path ctx ?f & args] (field path ctx ?f :read-only (read-only? ctx))))
     (fn [ctx]
       [(field [:link/rel] ctx read-only-cell)
        (field [:link/class] ctx read-only-cell)
        (field [:link/fiddle] ctx read-only-cell {:options "fiddle-options"
                                                  :option-label (r/comp pr-str :fiddle/ident first)})
        (field [:link/path] ctx read-only-cell)
        (field [:link/formula] ctx read-only-cell)
        (when-not embed-mode (field [:link/tx-fn] ctx read-only-cell))
        (when-not embed-mode (field [] ctx empty-renderer))])
     ctx
     props]))
