(ns hypercrud.browser.browser-request
  (:require
    [cats.monad.either :as either]
    [contrib.data :refer [unqualify]]
    [contrib.datomic]
    [contrib.reactive :as r]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hyperfiddle.data :as data]
    [hyperfiddle.route :as route]
    [taoensso.timbre :as timbre]))


(declare requests)

(defn requests-here [ctx]
  (doseq [link @(data/select-many-here ctx #{:hf/iframe})]
    (either/branch
      (base/browse-link+ ctx (r/pure link))
      (fn [e] (timbre/warn e))                                ; do we actually care about this error?
      requests)))

; at this point we only care about inline links and popovers are hydrated on their on hydrate-route calls
; On the request side, we walk the whole resultset and load each iframe from exactly the right place
; without any refocusing. Only on the view side do we care about drawing things in some other place.

(defn cross-streams [ctx]
  ; This does not get to look at the fiddlescope, though seems reasonable if it wanted to
  (if @(r/fmap :fiddle/hydrate-result-as-fiddle (:hypercrud.browser/fiddle ctx))
    ; This only makes sense on :fiddle/type :query because it has arbitrary arguments
    ; EntityRequest args are too structured.
    (let [[inner-fiddle & inner-args] (::route/datomic-args @(:hypercrud.browser/route ctx))
          route (cond-> {:hyperfiddle.route/fiddle inner-fiddle}
                  (seq inner-args) (assoc :hyperfiddle.route/datomic-args (vec inner-args)))]
      (either/branch
        (base/browse-route+ ctx route)
        (fn [e] (timbre/warn e))                                ; do we actually care about this error?
        requests))))

(defn request-attr-level [ctx]
  (doseq [[a ctx] (context/spread-attributes ctx)]
    (requests-here ctx)
    ; UnsupportedOperationException: Can only recur from tail position
    (request-attr-level ctx)))

; Spread across the resultset
; Create the links at each slot
; Index this by result-path
; index links by result-path
; [ctx route] (context/link slot :hf/iframe)


; Spread the slots
; at each slot foreach link Does the link match the slot?
; Accumulate a map of slot->#{links} (links is a sub index? {#{corcs}->link}
; At any slot it is very easy to know what the links are
; Very easy to refocus to another slot to get that link

(defn requests [ctx]
  ; More efficient to drive from links. But to do this, we need to refocus
  ; from the top, multiplying out for all possible dependencies.
  (requests-here ctx)
  (doseq [[_ ctx] (context/spread-result ctx)]
    (requests-here ctx)                                     ; depend on result? Not sure if right
    (doseq [[_ ctx] (context/spread-rows ctx)]
      #_(requests-here ctx)
      (doseq [[_ {el :hypercrud.browser/element :as ctx}] (context/spread-elements ctx)]
        (case (unqualify (contrib.datomic/parser-type @el))
          :variable nil #_(requests-here ctx)
          :aggregate nil #_(requests-here ctx)
          :pull (do (requests-here ctx)
                    ; Dependent attr links, slow af though, so disabled for now.
                    (request-attr-level ctx)))))

    ; no rows - independent
    (doseq [[_ {el :hypercrud.browser/element :as ctx}] (context/spread-elements ctx)]
      (case (unqualify (contrib.datomic/parser-type @el))
        :variable nil #_(requests-here ctx)
        :aggregate nil #_(requests-here ctx)
        :pull (do (requests-here ctx)
                  (request-attr-level ctx)))))
  (cross-streams ctx))
