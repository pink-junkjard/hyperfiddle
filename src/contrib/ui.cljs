(ns contrib.ui
  (:require
    [cats.monad.either :as either]
    [clojure.set :refer [rename-keys]]
    [contrib.cljs-platform :refer [global!]]
    [contrib.pprint :refer [pprint-str]]
    [contrib.reactive :as r]
    [contrib.reactive-debug :refer [track-cmp]]
    [contrib.string :refer [safe-read-edn-string blank->nil]]
    [contrib.ui.input :refer [adapt-props]]
    [contrib.ui.codemirror :refer [-codemirror camel-keys]]
    [contrib.ui.tooltip :refer [tooltip]]
    [contrib.ui.remark :as remark]
    [goog.object :as object]
    [re-com.core :as re-com]
    [reagent.core :as reagent]
    [taoensso.timbre :as timbre]))

(defn easy-checkbox [label value change! & [props]]
  (let [control [:input (adapt-props (merge props {:type "checkbox" :checked value :on-change change!}))]]
    (if (blank->nil label)
      [:label (merge props {:style {:font-weight "400"}})
       control " " label]
      control)))

(defn ^:export easy-checkbox-boolean [label r & [props]]
  [easy-checkbox label @r (r/partial swap! r not) props])

(defn ^:export code [value change! props]
  (let [defaults {:lineNumbers true
                  :matchBrackets true
                  :autoCloseBrackets true
                  :viewportMargin js/Infinity}
        ; Reagent as/element tampers with keys, we have to fix them
        props (merge defaults (camel-keys props))]
    ; There is nothing to be done about invalid css down here.
    ; You'd have to write CodeMirror implementation-specific css.
    [-codemirror value change! props]))

(defn ^:export code-inline-block [& args]
  (let [showing? (r/atom false)]
    (fn [value change! & [props]]
      [:div
       [re-com/popover-anchor-wrapper
        :showing? showing?
        :position :below-center
        :anchor [:a {:href "javascript:void 0;" :on-click (r/partial swap! showing? not)} "edit"]
        :popover [re-com/popover-content-wrapper
                  :close-button? true
                  :on-cancel (r/partial reset! showing? false)
                  :no-clip? true
                  :width "600px"
                  :body (code value change! props)]]
       " " value])))

(letfn [(edn-change! [change! user-edn-str]
          (-> (safe-read-edn-string user-edn-str)
              (either/branch
                (fn [e] (timbre/warn (pr-str e)) nil)       ; report error
                (fn [v] (change! v)))))]
  (defn -edn [code-control value change! props]
    ; Must validate since invalid edn means there's no value to stage.
    ; Code editors are different since you are permitted to stage broken code (and see the error and fix it)
    [code-control (pprint-str value) (r/partial edn-change! change!) props])) ; not reactive

(defn ^:export edn [value change! props]
  (-edn code value change! (assoc props :mode "clojure")))

(defn ^:export edn-inline-block [value change! props]
  (-edn code-inline-block value change! (assoc props :mode "clojure")))

(def ^:export ReactSlickSlider
  ; Prevents failure in tests, this is omitted from test preamble
  ; We don't have a way to differentiate tests-node from runtime-node, so check presence
  (if-let [reactSlickSlider (object/get (global!) "reactSlickSlider")]
    (reagent/adapt-react-class reactSlickSlider)))

(def ^:export ReactGifPlayer
  (if-let [ReactGifPlayer (object/get (global!) "ReactGifPlayer")]
    (reagent/adapt-react-class ReactGifPlayer)))

(def ^:export markdown (remark/remark!))

(defn radio-option [props]
  [tooltip {:label (:tooltip props)}
   [:label.radio-option {:class (if (:disabled props) "disabled")}
    [:input {:type "radio"
             :style {:width "auto"}
             :checked (= (:value props) (:target props))
             :on-change #((:change! props) (:target props))
             :disabled (:disabled props)}]
    (:label props)]])
