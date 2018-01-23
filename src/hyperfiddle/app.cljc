(ns hyperfiddle.app
  (:require [hyperfiddle.foundation :as foundation]))


; userland compat
(def ^:export alias? foundation/alias?)
(def ^:export hostname->hf-domain-name foundation/hostname->hf-domain-name)
(def ^:export process-domain foundation/process-domain-legacy)
