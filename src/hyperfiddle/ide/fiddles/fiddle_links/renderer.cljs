(ns hyperfiddle.ide.fiddles.fiddle-links.renderer
  (:require
    [cats.core :refer [mlet return]]
    [cats.monad.either :as either]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.system-link :refer [system-link?]]
    [hypercrud.client.core :as hc]
    [hyperfiddle.ui :refer [hyper-control field table link select]]
    [hyperfiddle.ui.select$ :refer [select-error-cmp]]))


(def editable-if-shadowed?
  #{:link/class :link/disabled? :link/fiddle :link/formula :link/tx-fn})

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
    (link :hf/new :fiddle ctx nil {:disabled (system-link? @(r/fmap :db/id (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))})
    (let [props (assoc props :read-only (read-only? ctx))]
      [select val ctx props])))

(letfn [(remove-children [field] (dissoc field :hypercrud.browser.field/children))]
  (defn hf-live-link-fiddle [val ctx props]
    (let [ctx (update ctx :hypercrud.browser/field #(r/fmap remove-children %))
          props (assoc props :read-only (read-only? ctx))]
      ; Hacks because hf-live is not yet modeled in the fiddle-graph, we hardcode knowledge of the IDE fiddle-graph instead
      (-> (mlet [req (base/meta-request-for-fiddle (assoc ctx
                                                     :route (hyperfiddle.ide/ide-fiddle-route (context/target-route ctx) ctx)
                                                     :branch nil))
                 topnav-fiddle @(hc/hydrate (:peer ctx) nil req) #_"todo tighter reactivity"]
            (return
              (let [ctx (merge ctx {:hypercrud.browser/links (r/track identity (:fiddle/links topnav-fiddle))})]
                [select val ctx props])))
          (either/branch select-error-cmp identity))
      )))

(let [empty-renderer (fn [val ctx props]
                       (link :hf/remove :link ctx "remove" {:disabled (system-link? @(r/fmap :db/id (:hypercrud.browser/data ctx)))}))]
  (defn renderer [val ctx {:keys [:embed-mode] :as props}]
    [table
     #_(partial form (fn [path ctx ?f & args] (field path ctx ?f :read-only (read-only? ctx))))
     (fn [ctx]
       [(when-not embed-mode (field [:link/disabled?] ctx read-only-cell))
        (field [:link/rel] ctx read-only-cell)
        (field [:link/class] ctx read-only-cell)
        (field [:link/fiddle] ctx (if embed-mode hf-live-link-fiddle link-fiddle) {:options "fiddle-options"
                                                                                   :option-label (r/comp pr-str :fiddle/ident first)})
        (field [:link/path] ctx read-only-cell)
        (field [:link/formula] ctx read-only-cell)
        (when-not embed-mode (field [:link/tx-fn] ctx read-only-cell))
        (when-not embed-mode (field [] ctx empty-renderer))])
     ctx
     props]))
