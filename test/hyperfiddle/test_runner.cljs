(ns hyperfiddle.test-runner
  (:require
    [doo.runner :refer-macros [doo-tests]]
    [reagent.dom.server]                                    ; needed for hypercrud.ui.safe-render

    ; NOT clj tests, this is only for CLJS tests

    contrib.char-test
    contrib.data-test
    #_contrib.datomic-errors-test
    contrib.datomic-tx-test
    contrib.eval-test
    contrib.reactive-test
    contrib.rfc3986-test
    contrib.string-test
    hypercrud.browser.auto-link-formula-test
    hypercrud.browser.auto-link-txfn-test
    hypercrud.browser.fiddle-test
    hypercrud.browser.find-element-test
    hypercrud.browser.router-test
    hypercrud.browser.system-link-test
    hypercrud.ui.control.link-controls-test
    hyperfiddle.ide.fiddles.schema_test
    hyperfiddle.ide-test
    hyperfiddle.readers-test
    ))

(defn run []
  (doo-tests
    'contrib.char-test
    'contrib.data-test
    #_'contrib.datomic-errors-test
    'contrib.datomic-tx-test
    'contrib.eval-test
    'contrib.reactive-test
    'contrib.rfc3986-test
    'contrib.string-test
    'hypercrud.browser.auto-link-formula-test
    'hypercrud.browser.auto-link-txfn-test
    'hypercrud.browser.fiddle-test
    'hypercrud.browser.find-element-test
    'hypercrud.browser.router-test
    'hypercrud.browser.system-link-test
    'hypercrud.ui.control.link-controls-test
    'hyperfiddle.ide.fiddles.schema_test
    'hyperfiddle.ide-test
    'hyperfiddle.readers-test
    ))
