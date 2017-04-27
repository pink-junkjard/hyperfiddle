(ns hypercrud.ui.renderer
  (:require [hypercrud.platform.safe-render :refer [safe-user-renderer]]
            [hypercrud.browser.links :as links]
            [hypercrud.compile.eval :refer [eval-str]]))


(defn empty-string-to-nil [s]
  (if (empty? s) nil s))

(defn user-renderer [param-ctx]
  (let [attr (:attribute param-ctx)]
    (or
      (empty-string-to-nil (get-in param-ctx [:fields (:attribute/ident attr) :renderer]))
      (empty-string-to-nil (-> attr :attribute/renderer))
      (empty-string-to-nil (-> attr :attribute/hc-type :hc-type/renderer)))))

(defn user-render [maybe-field anchors props param-ctx]
  (let [{user-fn :value error :error} (eval-str (user-renderer param-ctx))]
    [:div.value
     (if error
       [:div.value [:pre (pr-str error)]]
       (let [anchor-lookup (->> anchors
                                (mapv (juxt #(-> % :anchor/ident) identity))
                                (into {}))
             param-ctx (assoc param-ctx                     ; Todo unify link-fn with widget interface or something
                         :link-fn
                         (fn [ident label param-ctx]
                           (let [anchor (get anchor-lookup ident)
                                 props (links/build-link-props anchor param-ctx)] ; needs param-ctx to run formulas
                             [(:navigate-cmp param-ctx) props label])))]
         ; Same interface as auto-control widgets.
         ; pass value only as scope todo
         [safe-user-renderer user-fn maybe-field anchors props param-ctx]))]))
