(ns hyperfiddle.ide.ui.route-editor
  (:require
    [cats.monad.either :as either]
    [clojure.spec.alpha :as s]
    [contrib.pprint :refer [pprint-str]]
    [contrib.ui]
    [hypercrud.browser.base :as base]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]))


(defn- route-vec->route-map [[fiddle datomic-args service-args fragment]]
  {::route/fiddle fiddle
   ::route/datomic-args datomic-args
   ::route/service-args service-args
   ::route/fragment fragment})

(defn- route-map->route-vec [{:keys [::route/fiddle ::route/datomic-args ::route/service-args ::route/fragment]}]
  [fiddle datomic-args service-args fragment])

(defn- assert-new-v [m-route attr new-v]
  (->> (assoc m-route attr new-v)
       route-map->route-vec
       (apply route/canonicalize)
       (s/assert :hyperfiddle/route))
  new-v)

(defn- route-element [rt branch m-route attr]
  (let [parse-string (fn [s] (assert-new-v m-route attr (contrib.reader/read-edn-string! s)))
        to-string pprint-str]
    [contrib.ui/debounced
     {:value (get m-route attr)
      :on-change (fn [o n]
                   (->> (assoc m-route attr n)
                        route-map->route-vec
                        (apply route/canonicalize)
                        (runtime/set-route rt branch)))
      :mode "clojure"
      :lineNumbers false}
     contrib.ui/validated-cmp parse-string to-string contrib.ui/code]))

(defn- datomic-args [rt branch m-route]
  [route-element rt branch m-route ::route/datomic-args]
  #_(either/branch
    (base/hydrate-fiddle+ rt branch (::route/fiddle m-route))
    (fn [e]
      ; no fiddle for whatever reason, just hand the user a raw edn editor
      [route-element rt branch m-route ::route/datomic-args])
    (fn [fiddle]
      (case (:fiddle/type fiddle)
        :query (let []
                 [route-element rt branch m-route ::route/datomic-args])
        :entity (let []
                  [route-element rt branch m-route ::route/datomic-args])
        :blank [route-element rt branch m-route ::route/datomic-args]))))

(defn- form-row [label & content]
  [:div.row
   [:label.col-sm-2.col-form-label label]
   (into [:div.col-sm-10] content)])

(defn form-editor [rt branch]
  (let [m-route (route-vec->route-map (runtime/get-route rt branch))]
    [:form {:style {:padding "0 1rem" :background-color "#def"}}
     [form-row "fiddle" [route-element rt branch m-route ::route/fiddle]]
     [form-row "inputs" [datomic-args rt branch m-route]]
     ;[form-row "service args" [route-element rt branch m-route ::route/service-args]]
     [form-row "fragment" [route-element rt branch m-route ::route/fragment]]]))

(defn edn-editor [rt branch]
  [contrib.ui/debounced
   {:value (runtime/get-route rt branch)
    :on-change (fn [o n] (runtime/set-route rt branch n))
    :lineNumbers false}
   contrib.ui/cm-edn])
