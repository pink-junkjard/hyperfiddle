(ns hyperfiddle.test-runner
  (:require
    [doo.runner :refer-macros [doo-tests]]
    [reagent.dom.server]                                    ; needed for hypercrud.ui.safe-render

    contrib.char-test
    contrib.data-test
    contrib.datomic-tx-test
    contrib.eval-test
    contrib.reactive-test
    contrib.rfc3986-test
    hypercrud.browser.router-test
    hypercrud.ui.control.link-controls-test
    hyperfiddle.foundation-test
    hyperfiddle.ide-test
    hyperfiddle.readers-test
    ))

(defn run []
  (doo-tests
    'contrib.char-test
    'contrib.data-test
    'contrib.datomic-tx-test
    'contrib.eval-test
    'contrib.reactive-test
    'contrib.rfc3986-test
    'hypercrud.browser.router-test
    'hypercrud.ui.control.link-controls-test
    'hyperfiddle.foundation-test
    'hyperfiddle.ide-test
    'hyperfiddle.readers-test
    ))
