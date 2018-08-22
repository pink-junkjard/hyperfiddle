(ns hyperfiddle.core
  (:require
    ; public deps available inside query
    #?(:clj [contrib.datomic])

    ; public deps available for ui
    #?(:cljs [contrib.reagent])
    #?(:cljs [contrib.ui])

    ; public auto formulas
    [hypercrud.browser.auto-link-formula]

    #?(:cljs [hyperfiddle.ui])
    #?(:cljs [hyperfiddle.ui.hacks])

    ; These things can hardcode hyperfiddle.ui, like userland
    #?(:cljs [hyperfiddle.ui.markdown-extensions])
    #?(:cljs [hyperfiddle.ide.hf-live])
    ))

#?(:cljs (set! hyperfiddle.ui/markdown hyperfiddle.ui.markdown-extensions/markdown))

; WARNING:
; Do not import from within hyperfiddle namespaces.
; Import this only from main to avoid circular dependencies. Just list them all here.
