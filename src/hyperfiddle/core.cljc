(ns hyperfiddle.core
  (:require [hyperfiddle.foundation :as foundation]))


(def ^:export alias? foundation/alias?) ; userland compat
(def ^:export hostname->hf-domain-name foundation/hostname->hf-domain-name) ; userland compat
