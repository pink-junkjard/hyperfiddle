(ns hyperfiddle.test-runner
  (:require
    [doo.runner :refer-macros [doo-tests]]

    ; NOT clj tests, this is only for CLJS tests
    contrib.char$-test
    contrib.data-test
    contrib.datomic-errors-test
    contrib.datomic-test
    contrib.datomic-tx-test
    contrib.ednish-test
    contrib.eval-test
    contrib.match-test
    contrib.pprint-test
    contrib.reactive-test
    contrib.rfc3986-test
    contrib.string-test
    contrib.validation-test
    hypercrud.browser.field-test
    hypercrud.browser.nested-pull-test
    hypercrud.browser.router-bidi-test
    hyperfiddle.data-test
    hyperfiddle.fiddle-test
    hyperfiddle.ide.console-links-test
    hyperfiddle.io.global-basis-test
    hyperfiddle.readers-test
    hyperfiddle.route-test
    hyperfiddle.ui.docstring-test
    ))

(defn run []
  (doo-tests
    'contrib.char$-test
    'contrib.data-test
    'contrib.datomic-errors-test
    'contrib.datomic-test
    'contrib.datomic-tx-test
    'contrib.ednish-test
    'contrib.eval-test
    'contrib.match-test
    'contrib.pprint-test
    'contrib.reactive-test
    'contrib.rfc3986-test
    'contrib.string-test
    'contrib.validation-test
    'hypercrud.browser.field-test
    'hypercrud.browser.nested-pull-test
    'hypercrud.browser.router-bidi-test
    'hyperfiddle.data-test
    'hyperfiddle.fiddle-test
    'hyperfiddle.ide.console-links-test
    'hyperfiddle.io.global-basis-test
    'hyperfiddle.readers-test
    'hyperfiddle.route-test
    'hyperfiddle.ui.docstring-test
    ))
