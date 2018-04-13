(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require [cats.monad.either :as either]
            [contrib.base-64-url-safe :as base64-url-safe]
            [contrib.data :refer [kwargs unwrap]]
            [contrib.datomic-tx :as tx]
            [contrib.reactive :as reactive]
            [contrib.reagent :refer [fragment]]
            [hypercrud.browser.browser-ui :as browser-ui]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.link :as link]
            [hypercrud.browser.system-fiddle :as system-fiddle]
            [hypercrud.ui.control.markdown-rendered :refer [markdown]]
            [hypercrud.ui.error :as ui-error]
            [hypercrud.ui.radio :as radio]
            [hypercrud.ui.result :as result]
            [hypercrud.ui.tooltip :refer [tooltip]]
            [hyperfiddle.foundation :as foundation :refer [staging]]
            [hyperfiddle.foundation.actions :as foundation-actions]
            [hyperfiddle.ide.fiddles.topnav-bindings :as topnav-bindings]
            [hyperfiddle.runtime :as runtime]))


(defn fiddle-src-renderer [ctx]
  [:pre (pr-str (keys ctx))])
