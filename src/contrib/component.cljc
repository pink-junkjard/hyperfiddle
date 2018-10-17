(ns contrib.component)                                      ; todo stuartsierra/component


(defprotocol Lifecycle
  (start [component])
  (stop [component]))
