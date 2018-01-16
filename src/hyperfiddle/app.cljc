(ns hyperfiddle.app
  (:require [hyperfiddle.appval.domain.core :as foundation-core]
            [hyperfiddle.appval.domain.foundation :as foundation]))


; userland compat
(def ^:export alias? hyperfiddle.appval.domain.core/alias?)
(def ^:export hostname->hf-domain-name foundation-core/hostname->hf-domain-name)
(def ^:export process-domain foundation/process-domain)
