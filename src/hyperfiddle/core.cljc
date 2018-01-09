(ns hyperfiddle.core
  (:require [hyperfiddle.appval.domain.core]))


(def ^:export alias? hyperfiddle.appval.domain.core/alias?)          ; userland compat
(def ^:export hostname->hf-domain-name hyperfiddle.appval.domain.core/hostname->hf-domain-name) ; userland compat
