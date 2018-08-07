(ns hypercrud.browser.link
  (:require
    [cats.core :as cats]
    [cats.monad.either :as either]
    [contrib.data :refer [abs-normalized unwrap]]
    [contrib.eval :as eval]
    [contrib.pprint :refer [pprint-str]]
    [contrib.reactive :as r]
    [contrib.string :refer [memoized-safe-read-edn-string]]
    [contrib.try$ :refer [try-either]]
    [cuerdas.core :as string]
    [hypercrud.browser.auto-link-formula :as auto-link-formula]
    [hypercrud.browser.routing :as routing]
    [hypercrud.util.branch :as branch]
    [taoensso.timbre :as timbre]))


(defn popover-link? [link] (:link/managed? link))

(defn- read-path [s]
  (either/branch
    (memoized-safe-read-edn-string (str "[" s "]"))
    #(do (timbre/error %) nil)                              ; too late to report anything to the dev
    identity))

(defn same-path-as? [path link]
  (= path (read-path (:link/path link))))

(defn draw-link? "Full semantics unclear" [path link]
  (let [classes (->> (:link/class link) (map read-path) (remove nil?) (into #{}))]
    (if (empty? classes)
      (= path (read-path (:link/path link)))
      (contains? classes path))))

(defn links-at [path links-ref]
  (filter (partial same-path-as? path) @links-ref))

(defn select-all-at-path [ctx rel]
  (->> (filter #(= (:link/rel %) rel) @(:hypercrud.browser/links ctx))
       (filter (partial same-path-as? (:hypercrud.browser/path ctx)))))

(defn rel->link "doesn't validate" [rel ctx]
  (first (select-all-at-path ctx rel)))

(defn same-class-as? [class {classes :link/class}]
  (boolean ((set classes) class)))

(defn select-all "Find the closest match. Can it search parent scopes for :options ?"
  ([ctx rel] {:pre [rel]}
   (->> @(:hypercrud.browser/links ctx)
        (filter #(= rel (:link/rel %)))))
  ([ctx rel ?class]
   (->> (select-all ctx rel)
        (filter (fn [link]
                  (if ?class
                    (same-class-as? ?class link)
                    true)))))
  ([ctx rel ?class ?path]
   (->> (select-all ctx rel ?class)
        (filter (fn [link]
                  (if ?path
                    (draw-link? ?path link)
                    true))))))

(comment
  ; Maybe this shouldn't exist, the caller should validate?
  (defn select-one [ctx rel & [?class]]
    (first (select-all ctx rel ?class))))

(let [memoized-eval-props (memoize eval/safe-eval-string)]
  (defn eval-hc-props [props-str ctx]
    (if (and (string? props-str) (not (string/blank? props-str)))
      (cats/bind (memoized-eval-props props-str)
                 (fn [f-or-v]
                   (if (fn? f-or-v)
                     (try-either (f-or-v ctx))
                     (either/right f-or-v))))
      (either/right nil))))

(defn ^:export build-link-props-raw [unvalidated-route' link ctx] ; ctx is for display-mode
  ; this is a fine place to eval, put error message in the tooltip prop
  ; each prop might have special rules about his default, for example :visible is default true, does this get handled here?
  (let [fiddle (:link/fiddle link)                          ; can be nil - in which case route is invalid
        [_ args :as route] (unwrap unvalidated-route')
        validated-route' (routing/validated-route' fiddle route ctx)
        user-props' (eval-hc-props (:hypercrud/props link) ctx)
        user-props (unwrap user-props')
        errors (->> [user-props' unvalidated-route' validated-route']
                    (filter either/left?) (map cats/extract) (into #{}))]
    (merge
      user-props                                            ; e.g. disabled, tooltip, style, class - anything, it gets passed to a renderer maybe user renderer
      ; doesn't handle tx-fn - meant for the self-link. Weird and prob bad.
      {:route (unwrap unvalidated-route')
       ; todo tooltip and class dont belong
       :tooltip (if-not (empty? errors)
                  [:warning (pprint-str errors)]
                  (if (:ide-active ctx)
                    [nil (pr-str args)]
                    (:tooltip user-props)))
       :class (->> [(:class user-props)
                    (if-not (empty? errors) "invalid")]
                   (remove nil?)
                   (interpose " ")
                   (apply str))})))

; if this is driven by link, and not route, it needs memoized.
; the route is a fn of the formulas and the formulas can have effects
; which have to be run only once.
(defn build-link-props [link ctx & [dont-branch?]]          ; this 'dont-branch?' argument is a holdover for topnav until 'iframe/button/anchor'
  ; Draw as much as possible even in the presence of errors, still draw the link, collect all errors in a tooltip.
  ; Error states:
  ; - no route
  ; - invalid route
  ; - broken user formula
  ; - broken user txfn
  ; - broken user visible fn
  ; If these fns are ommitted (nil), its not an error.
  (let [route' (routing/build-route' link ctx)
        hypercrud-props (build-link-props-raw route' link ctx)
        popover-props (when (popover-link? link)
                        (let [child-branch (let [child-id-str (-> [(auto-link-formula/deterministic-ident ctx) (:db/id link)]
                                                                  hash abs-normalized - str)]
                                             (branch/encode-branch-child (:branch ctx) child-id-str))]
                          ; we should run the auto-formula logic to determine an appropriate auto-id fn
                          {:popover-id child-branch         ; just use child-branch as popover-id
                           :child-branch (when-not dont-branch? child-branch)
                           ; todo clean up this type and stabalize reactions
                           :link (r/track identity link)}))]
    (merge hypercrud-props {:popover popover-props})))
