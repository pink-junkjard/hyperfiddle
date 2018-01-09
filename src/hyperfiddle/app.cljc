(ns hyperfiddle.app
  (:require hyperfiddle.appval.domain.core
            hyperfiddle.appval.domain.app))


(def ^:export alias? hyperfiddle.appval.domain.core/alias?)          ; userland compat
(def ^:export hostname->hf-domain-name hyperfiddle.appval.domain.core/hostname->hf-domain-name) ; userland compat
(def ^:export process-domain hyperfiddle.appval.domain.app/process-domain) ; userland compat
