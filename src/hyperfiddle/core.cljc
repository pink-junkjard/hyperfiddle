(ns hyperfiddle.core
  (:require
    ; public deps available inside query
    #?(:clj [contrib.datomic])

    ; public deps available for ui
    #?(:cljs [contrib.reagent])
    #?(:cljs [contrib.ui])
    #?(:cljs [hyperfiddle.ui])
    #?(:cljs [hyperfiddle.ide.hf-live])
    ))


; WARNING:
; Do not import from within hyperfiddle namespaces.
; Import this only from main to avoid circular dependencies. Just list them all here.
