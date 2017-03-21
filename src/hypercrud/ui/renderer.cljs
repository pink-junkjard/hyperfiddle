(ns hypercrud.ui.renderer
  (:require [reagent.core :as reagent]
            [reagent.impl.component :as r-comp]
            [hypercrud.browser.links :as links]
            [hypercrud.compile.eval :refer [eval-str]]))

(defn safe-render! [this]
  (let [[react-ctor & cmp-and-props] (r-comp/get-argv this)
        dom-el (reagent/dom-node this)]
    (try
      ; construct hiccup markup for reagent lolz
      ; Hack in a key because these are simple user-fns, they can't construct proper reagent classes
      ; with lifecycle, yet.
      (reagent/render (with-meta (vec cmp-and-props) {:key (rest cmp-and-props)}) dom-el)
      (catch js/Error e
        (reagent/render [:pre (pr-str e)] dom-el)))))

(def safe-user-renderer
  (reagent/create-class
    {:reagent-render (fn [user-fn & props] [:div])          ; portal container
     :component-did-mount safe-render!
     :component-will-unmount
     (fn [this]
       (reagent/unmount-component-at-node (reagent/dom-node this)))
     :component-did-update safe-render!}))

(defn empty-string-to-nil [s]
  (if (empty? s) nil s))

(defn user-renderer [param-ctx]
  (let [attr (:attribute param-ctx)]
    (or
      (empty-string-to-nil (get-in param-ctx [:fields (:attribute/ident attr) :renderer]))
      (empty-string-to-nil (-> attr :attribute/renderer))
      (empty-string-to-nil (-> attr :attribute/hc-type :hc-type/renderer)))))

(defn user-render [value maybe-field anchors props param-ctx]
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
         [safe-user-renderer user-fn value maybe-field anchors props param-ctx]))]))
