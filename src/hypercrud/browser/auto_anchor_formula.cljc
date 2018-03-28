(ns hypercrud.browser.auto-anchor-formula
  (:require [hypercrud.browser.auto-link-formula :as auto-link-formula]
            [taoensso.timbre :as timbre]))


(defn ^:deprecated auto-entity-from-stage [& args]
  (timbre/error "Warning: hypercrud.browser.auto-anchor-formula is deprecated and will be removed in the future.  Please use hypercrud.browser.auto-link-formula/auto-entity-from-stage")
  (apply auto-link-formula/auto-entity-from-stage args))

(defn ^:deprecated auto-entity [& args]
  (timbre/error "Warning: hypercrud.browser.auto-anchor-formula is deprecated and will be removed in the future.  Please use hypercrud.browser.auto-link-formula/auto-entity")
  (apply auto-link-formula/auto-entity args))
