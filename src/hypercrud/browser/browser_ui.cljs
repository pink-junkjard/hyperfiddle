(ns hypercrud.browser.browser-ui
  (:require
    [hypercrud.browser.router :as router]))


(defn frame-on-click [rt branch branch-aux route event]
  (when route                                               ; under what circimstances is this nil?
    (let [is-alt-pressed (.-altKey event)]
      (when is-alt-pressed
        (let [anchor (-> (.composedPath event) (aget 0) (.matches "a"))
              anchor-descendant (-> (.composedPath event) (aget 0) (.matches "a *"))]
          (when-not (or anchor anchor-descendant)
            (.stopPropagation event)
            (js/window.open (router/encode route) "_blank")))))))
