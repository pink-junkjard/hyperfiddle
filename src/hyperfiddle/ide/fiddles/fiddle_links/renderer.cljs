(ns hyperfiddle.ide.fiddles.fiddle-links.renderer
  (:require
    [contrib.reactive :as r]
    [contrib.reagent :refer [from-react-context with-react-context]]
    [hypercrud.browser.system-link :refer [system-link?]]
    [hyperfiddle.data :refer [sort-fn]]
    [hyperfiddle.ui :refer [hyper-control field table]]))


(def editable-if-shadowed?
  #{:link/disabled? :link/render-inline? :link/fiddle :link/formula :link/tx-fn :hypercrud/props})

(defn read-only? [ctx]
  (if (:hypercrud.browser/data ctx)                         ; be robust to being called on labels
    (let [entity (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])
          sys? (system-link? @(r/fmap :db/id entity))
          shadow? @(r/fmap :hypercrud/sys? entity)]
      (or sys? (and shadow? (editable-if-shadowed? (:hypercrud.browser/attribute ctx)))))))

(def read-only-cell
  (from-react-context
    (fn [{:keys [ctx props]} value]
      ; Need to delay until we have the value ctx to compute this, which means its a value renderer not a field prop
      [with-react-context {:ctx ctx
                           ; rebind updated props
                           :props (assoc props :read-only (read-only? ctx))}
       [(hyper-control ctx) value]])))

(defn renderer [ctx]
  [table
   #_(partial form (fn [path ctx ?f & args] (field path ctx ?f :read-only (read-only? ctx))))
   (fn [ctx]
     [(field [:link/disabled?] ctx read-only-cell)
      (field [:link/rel] ctx read-only-cell)
      (field [:link/path] ctx read-only-cell)
      (field [:link/render-inline?] ctx read-only-cell)
      (field [:link/fiddle] ctx read-only-cell)
      (field [:link/create?] ctx read-only-cell)
      (field [:link/managed?] ctx read-only-cell)
      (field [:link/formula] ctx read-only-cell)
      (field [:link/tx-fn] ctx read-only-cell)
      (field [:hypercrud/props] ctx read-only-cell)
      #_(field [] ctx nil)])
   sort-fn
   ctx])
