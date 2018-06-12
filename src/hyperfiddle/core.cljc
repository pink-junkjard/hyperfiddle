(ns hyperfiddle.core
  (:require
    [hypercrud.ui.result]

    ; pull in public ui deps
    #?(:cljs [contrib.reagent])
    #?(:cljs [contrib.ui])
    #?(:cljs [contrib.ui.radio])
    #?(:cljs [hypercrud.ui.textarea])

    #?(:cljs [hyperfiddle.ui])
    ;user land (todo these should be in a core hc.ui namespace; widget is arbitrary)
    #?(:cljs [hypercrud.ui.attribute.code])
    #?(:cljs [hypercrud.ui.attribute.checkbox])
    #?(:cljs [hypercrud.ui.attribute.markdown-editor])
    #?(:cljs [hypercrud.ui.attribute.tristate-boolean])
    ))


; WARNING:
; Do not import from within hyperfiddle namespaces.
; Import this only from main to avoid circular dependencies. Just list them all here.
