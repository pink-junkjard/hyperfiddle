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
      (reagent/render (vec cmp-and-props) dom-el)
      (catch js/Error e
        (reagent/render [:pre (pr-str e)] dom-el)))))

(def safe-user-renderer
  (reagent/create-class
    {:reagent-render (fn [user-fn & props] [:div]) ; portal container
     :component-did-mount safe-render!
     :component-will-unmount
     (fn [this]
       (reagent/unmount-component-at-node (reagent/dom-node this)))
     :component-did-update safe-render!}))

(defn empty-string-to-nil [s]
  (if (empty? s) nil s))

(defn renderer-for-attribute [attribute]
  (or (empty-string-to-nil (:attribute/renderer attribute))
      (empty-string-to-nil (-> attribute :attribute/hc-type :hc-type/renderer))))

(defn attribute-renderer [value maybe-field anchors props param-ctx]
  (let [{user-fn :value error :error} (eval-str (renderer-for-attribute (:attribute param-ctx)))]
    [:div.value
     (if error
       [:div.value [:pre (pr-str error)]]
       (let [anchor-lookup (->> anchors
                                (mapv (juxt #(-> % :anchor/ident) identity))
                                (into {}))
             ; Todo unify link-fn with widget interface or something
             link-fn (fn [ident label param-ctx]
                       (let [anchor (get anchor-lookup ident)
                             props (links/build-link-props anchor param-ctx)]
                         [(:navigate-cmp param-ctx) props label param-ctx]))]
         ; Same interface as auto-control widgets.
         [safe-user-renderer user-fn value maybe-field anchors props param-ctx]))]))
