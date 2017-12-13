(ns hypercrud.browser.auto-link
  (:require [hypercrud.browser.auto-fiddle :as auto-fiddle]
            [taoensso.timbre :as timbre]))


(defn system-link? [& args]
  (timbre/error "Warning: hypercrud.browser.auto-link/system-link? is deprecated and will be removed. Use hypercrud.browser.auto-fiddle/system-link?")
  (apply auto-fiddle/system-fiddle? args))
