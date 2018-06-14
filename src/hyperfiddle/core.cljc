(ns hyperfiddle.core
  (:require
    ; pull in public ui deps
    #?(:cljs [contrib.reagent])
    #?(:cljs [contrib.ui])
    #?(:cljs [hyperfiddle.ui])
    #?(:cljs [hypercrud.ui.attribute.markdown-editor])      ; legacy
    #?(:cljs [hypercrud.ui.attribute.code])                 ; legacy
    #?(:cljs [hypercrud.ui.result])                         ; legacy
    ))


; WARNING:
; Do not import from within hyperfiddle namespaces.
; Import this only from main to avoid circular dependencies. Just list them all here.
