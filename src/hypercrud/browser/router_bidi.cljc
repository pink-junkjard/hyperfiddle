(ns hypercrud.browser.router-bidi
  (:require
    [bidi.bidi :as bidi]
    [contrib.reader :refer [read-edn-string]]
    [contrib.string :refer [abc]]
    [hypercrud.browser.system-fiddle :refer [system-fiddle?]]
    [hypercrud.types.ThinEntity :refer [->ThinEntity #?(:cljs ThinEntity)]])
  #?(:clj
     (:import (hypercrud.types.Entity Entity)
              (hypercrud.types.ThinEntity ThinEntity))))


; Bidi routing protocols
(def regex-keyword "^:[A-Za-z]+[A-Za-z0-9\\*\\+\\!\\-\\_\\?\\.]*(?:%2F[A-Za-z]+[A-Za-z0-9\\*\\+\\!\\-\\_\\?\\.]*)?")
(def regex-string "^[A-Za-z0-9\\-\\_\\.]+")
(def regex-long "^[0-9]+")

(extend-type ThinEntity

  ; must be safe to user input. if they throw, the app won't fallback to a system-route

  bidi/PatternSegment
  (segment-regex-group [_]
    ; the value mathign the placeholder can be a keyword or a long
    (str "(?:" regex-long ")|(?:" regex-keyword ")"))
  (param-key [this]
    ; Abusing .-id to store the param-key in the entity placeholder in the route
    (.-id this))
  (transform-param [this]
    (fn [v]
      (let [$ (.-dbname this)                               ; the "$" is provided by entity placeholder in the route
            e (read-edn-string v)]                          ; the reader will need to subs ! to /
        (->ThinEntity $ e))))
  (matches? [this s]
    (let [r (re-pattern
              "[A-Za-z]+[A-Za-z0-9\\*\\+\\!\\-\\_\\?\\.]*(?:%2F[A-Za-z]+[A-Za-z0-9\\*\\+\\!\\-\\_\\?\\.]*)?")]
      (re-matches r s)))
  (unmatch-segment [this params]
    (let [entity (get params (.-id this))]
      ; lookup refs not implemented, but eid and ident work
      ; safe
      (some-> entity .-id)))

  bidi/Pattern
  (match-pattern [this env]
    ; is this even in play? I don't think I ever hit this bp
    (let [read (read-edn-string (:remainder env))]
      (-> env
          (update-in [:route-params] assoc (.-id this) (->ThinEntity (.-dbname this) read))
          ; totally not legit to count read bc whitespace
          (assoc :remainder (subs (:remainder env) 0 (count (pr-str read)))))))
  (unmatch-pattern [this m]
    (let [param-key (.-id this)]
      (-> m :params (get param-key) .-id pr-str)))

  ;bidi/Matched
  ;(resolve-handler [this m] (bidi/succeed this m))
  ;(unresolve-handler [this m] (when (= this (:handler m)) ""))
  )


; Bidi is not a great fit for the sys router because there is only one route with varargs params
; params are dynamically typed; need to model an `any`, eager-consume the path segment,
; and figure out the edn from the path segment. How to determine #entity vs scalar?
;(def sys-router
;  [["/" :fiddle-id "/" #edn 0] :browse])

(defn ->bidi-consistency-wrapper [{:keys [handler route-params] :as ?r}]
  ; Bidi's interface is inconsistent and makes you understand two ways to identify a route
  ; "bidi/match" return val syntax, is the bad one
  ; Canonicalize on the "bidi/path-for" syntax
  (if ?r (apply conj [handler] (mapcat identity route-params))))

(defn bidi->hf [[handler & ?route-params :as ?r]]
  (if ?r
    [handler
     (->> ?route-params                                     ; bidi gives us alternating k/v
          (partition-all 2)
          (map vec)
          sort                                              ; order by keys for hyperfiddle, router should use kw or int
          (mapv second)                                     ; drop keys; hyperfiddle params are associative by index
          )]))

(defn bidi-match->path-for "adapt bidi's inconsistent interface" [[h & ps :as ?r]]
  (if ?r {:handler h :route-params ps}))

(defn ->bidi [[fiddle args :as ?r]]
  (assert (not (system-fiddle? fiddle)) "bidi router doesn't handle sys links")
  ; this is going to generate param names of 0, 1, ... which maybe doesn't work for all routes
  ; we would need to disallow bidi keywords for this to be valid. Can bidi use ints? I think not :(
  (if ?r (apply conj [fiddle] (mapcat vector (abc) args))))

