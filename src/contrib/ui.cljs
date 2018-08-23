(ns contrib.ui
  (:require
    [clojure.set :refer [rename-keys]]
    [contrib.cljs-platform :refer [global!]]
    [contrib.data :as util]
    [contrib.pprint :refer [pprint-str]]
    [contrib.reactive :as r]
    [contrib.reader]
    [contrib.string :refer [blank->nil]]
    [contrib.ui.input :refer [adapt-props]]
    [contrib.ui.codemirror :refer [-codemirror camel-keys]]
    [contrib.ui.tooltip :refer [tooltip]]
    [contrib.ui.remark :as remark]
    [goog.object :as object]
    [re-com.core :as re-com]
    [reagent.core :as reagent]))

(defn easy-checkbox [label value change! & [props]]
  (let [control [:input (adapt-props (merge props {:type "checkbox" :checked value :on-change change!}))]]
    (if (blank->nil label)
      [:label (merge props {:style {:font-weight "400"}})
       control " " label]
      control)))

(defn ^:export easy-checkbox-boolean [label r & [props]]
  [easy-checkbox label @r (r/partial swap! r not) props])

(defn ^:export code [props]
  (let [defaults {:lineNumbers true
                  :matchBrackets true
                  :autoCloseBrackets true
                  :viewportMargin js/Infinity}
        ; Reagent as/element tampers with keys, we have to fix them
        props (merge defaults props #_(camel-keys props))]
    ; There is nothing to be done about invalid css down here.
    ; You'd have to write CodeMirror implementation-specific css.
    [-codemirror props]))

(defn ^:export code-inline-block [props]
  (let [showing? (r/atom false)]
    (fn [props]
      [:div.truncate
       [re-com/popover-anchor-wrapper
        :showing? showing?
        :position :below-center
        :anchor [:a {:href "javascript:void 0;" :on-click (r/partial swap! showing? not)} "edit"]
        :popover [re-com/popover-content-wrapper
                  :close-button? true
                  :on-cancel (r/partial reset! showing? false)
                  :no-clip? true
                  :width "600px"
                  :body (code props)]]
       " " [:code (:value props)]])))

(letfn [(read-edn-string [user-edn-str]
          ; parent on-change can catch exceptions if they care
          ; otherwise this will bubble up to the console appropriately
          (some-> user-edn-str contrib.reader/read-edn-string))]
  (defn- adapt-edn-props [props]
    ; Must validate since invalid edn means there's no value to stage.
    ; Code editors are different since you are permitted to stage broken code (and see the error and fix it)
    (-> props
        (assoc :mode "clojure")
        (update :value pprint-str)
        (util/update-existing :on-change r/comp read-edn-string))))

(defn ^:export edn [props] [code (adapt-edn-props props)])

(defn ^:export edn-inline-block [props] [code-inline-block (adapt-edn-props props)])

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
