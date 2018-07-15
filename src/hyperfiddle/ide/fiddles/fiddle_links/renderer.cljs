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
      (or sys? (and shadow? (not (editable-if-shadowed? (:hypercrud.browser/attribute ctx))))))))

(def read-only-cell
  (from-react-context
    (fn [{:keys [ctx props]} value]
      ; Need to delay until we have the value ctx to compute this, which means its a value renderer not a field prop
      [with-react-context {:ctx ctx
                           ; rebind updated props
                           :props (assoc props :read-only (read-only? ctx))}
       [(hyper-control ctx) value]])))


(letfn [(remove-children [field] (dissoc field :hypercrud.browser.field/children))]
  (def hf-live-link-fiddle
    (from-react-context
      (fn [{:keys [ctx props]} value]
        (let [ctx (-> ctx
                      (update :hypercrud.browser/field #(r/fmap remove-children %))
                      (assoc :hypercrud.browser/fields (r/track identity nil)))]
          [with-react-context {:ctx ctx
                               :props (assoc props :read-only (read-only? ctx))}
           [(hyper-control ctx) value]])))))

(defn renderer [ctx & [embed-mode]]
  [table
   #_(partial form (fn [path ctx ?f & args] (field path ctx ?f :read-only (read-only? ctx))))
   (fn [ctx]
     [(when-not embed-mode (field [:link/disabled?] ctx read-only-cell))
      (field [:link/rel] ctx read-only-cell)
      (field [:link/path] ctx read-only-cell)
      (field [:link/render-inline?] ctx read-only-cell)
      (field [:link/fiddle] ctx (if embed-mode hf-live-link-fiddle read-only-cell))
      (when-not embed-mode (field [:link/create?] ctx read-only-cell))
      (when-not embed-mode (field [:link/managed?] ctx read-only-cell))
      (field [:link/formula] ctx read-only-cell)
      (when-not embed-mode (field [:link/tx-fn] ctx read-only-cell))
      (when-not embed-mode (field [:hypercrud/props] ctx read-only-cell))
      (when-not embed-mode (field [] ctx nil))])
   sort-fn
   ctx])
