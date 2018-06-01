(ns hypercrud.browser.browser-ui-test
  (:require [clojure.test :refer [deftest is]]
            [contrib.eval :as eval]
            [hypercrud.browser.browser-ui :as browser-ui]
            [reagent.dom.server :as dom-server]))


(defn test-renderer-str [user-str & args]
  (let [f (eval/eval-string (browser-ui/build-wrapped-render-expr-str user-str))]
    (dom-server/render-to-static-markup (into [f] args))))
