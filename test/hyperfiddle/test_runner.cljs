(ns hyperfiddle.test-runner
  (:require
    [clojure.test :refer [deftest is]]
    [doo.runner :refer-macros [doo-tests]]
    [reagent.dom.server]                                    ; needed for hypercrud.ui.safe-render

    hypercrud.client.tx-test
    contrib.eval-test
    hyperfiddle.readers-test
    hypercrud.ui.control.link-controls-test
    hyperfiddle.foundation-test
    hyperfiddle.ide-test
    hypercrud.browser.router-test
    contrib.data-test
    contrib.char-test
    contrib.rfc3986-test
    ))

(defn run []
  (doo-tests
    'hypercrud.client.tx-test
    'contrib.eval-test
    'hyperfiddle.readers-test
    'hypercrud.ui.control.link-controls-test
    'hyperfiddle.foundation-test
    'hyperfiddle.ide-test
    'hypercrud.browser.router-test
    'contrib.data-test
    'contrib.char-test
    'contrib.rfc3986-test))
