(ns hypercrud.browser.browser-ui
  (:require
    [contrib.css :refer [css-slugify css]]
    [contrib.reagent-native-events :refer [button=]]
    [contrib.rfc3986 :refer [encode-rfc3986-pchar]]
    [contrib.ednish :refer [encode-ednish]]
    [hypercrud.browser.router :as router]))


(defn page-on-click "middle-click will open in new tab; alt-middle-click will open srcmode in new tab"
  [rt branch branch-aux route event]
  (when route                                               ; under what circimstances is this nil?
    (let [is-alt-pressed (.-altKey event)]
      (when is-alt-pressed
        (let [anchor (-> (.composedPath event) (aget 0) (.matches "a"))
              anchor-descendant (-> (.composedPath event) (aget 0) (.matches "a *"))]
          (when-not (or anchor anchor-descendant)
            (let [route (router/assoc-frag route (encode-rfc3986-pchar (encode-ednish (pr-str :src))))]
              (.stopPropagation event)
              (js/window.open (router/encode route) "_blank"))

            ; Leaving this imperative mess in vcs because it was hard to write
            #_(let [open-new-tab (or (button= :middle event) (and (button= :left event) (.-metaKey event)))]
                (when-not open-new-tab
                  ; Do we even need to drill down into this tab under any circumstances? I suspect not.
                  ; Can always middle click and then close this tab.
                  #_#_:else (runtime/dispatch! rt (fn [dispatch! get-state]
                                                    (when (foundation/navigable? route (get-state))
                                                      (actions/set-route rt route branch false false dispatch! get-state))))))))))))
