(ns hyperfiddle.ide.ui.route-editor
  (:require
    [clojure.spec.alpha :as s]
    [contrib.ui]
    [contrib.pprint :refer [pprint-str]]
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

(defn- route-element [rt branch m-route label attr]
  (let [parse-string (fn [s] (assert-new-v m-route attr (contrib.reader/read-edn-string! s)))
        to-string pprint-str]
    [:div.row
     [:label.col-sm-2.col-form-label label]
     [:div.col-sm-10
      [contrib.ui/debounced
       {:value (get m-route attr)
        :on-change (fn [o n]
                     (->> (assoc m-route attr n)
                          route-map->route-vec
                          (apply route/canonicalize)
                          (runtime/set-route rt branch)))
        :mode "clojure"
        :lineNumbers false}
       contrib.ui/validated-cmp parse-string to-string contrib.ui/code]]]))

(defn form-editor [rt branch]
  (let [m-route (route-vec->route-map (runtime/get-route rt branch))]
    [:form {:style {:padding "0 1rem" :background-color "#def"}}
     [route-element rt branch m-route "fiddle" ::route/fiddle]
     [route-element rt branch m-route "inputs" ::route/datomic-args]
     ;[route-element rt branch m-route "service args" ::route/service-args]
     [route-element rt branch m-route "fragment" ::route/fragment]]))

(defn edn-editor [rt branch]
  [contrib.ui/debounced
   {:value (runtime/get-route rt branch)
    :on-change (fn [o n] (runtime/set-route rt branch n))
    :lineNumbers false}
   contrib.ui/cm-edn])
