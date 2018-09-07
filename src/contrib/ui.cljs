(ns contrib.ui
  (:require
    [clojure.set :refer [rename-keys]]
    [contrib.cljs-platform :refer [global!]]
    [contrib.data :refer [update-existing]]
    [contrib.pprint :refer [pprint-str]]
    [contrib.reactive :as r]
    [contrib.reader]
    [contrib.string :refer [blank->nil]]
    [contrib.ui.input :as input]
    [contrib.ui.codemirror :refer [-codemirror]]
    [contrib.ui.tooltip :refer [tooltip]]
    [contrib.ui.remark :as remark]
    [goog.functions :as functions]
    [goog.object :as object]
    [re-com.core :as re-com]
    [reagent.core :as reagent]
    [taoensso.timbre :as timbre]))


(def default-debounce-ms 250)

(let [debounce (memoize functions/debounce)
      debounced-adapter (fn [os-ref f n]
                          (let [o (last (:old-values @os-ref))]
                            (swap! os-ref update :old-values conj n)
                            (when f (f o n))))
      on-change (fn [os-ref n]                              ; letfn not working #470
                  (swap! os-ref assoc :value n)
                  n)]
  (defn debounced [props comp & args]
    (let [os-ref (r/atom {:value (:value props)
                          :old-values [(:value props)]})]
      (reagent/create-class
        {:reagent-render
         (fn [props comp & args]
           (let [props (-> props
                           (dissoc :debounce/interval)
                           (assoc :value @(r/cursor os-ref [:value]))
                           (update :on-change (fn [f]
                                                (let [f (debounce (r/partial debounced-adapter os-ref f)
                                                                  (or (:debounce/interval props) default-debounce-ms))]
                                                  (r/comp f (r/partial on-change os-ref))))))]
             (into [comp props] args)))
         :component-did-update
         (fn [this]
           (let [[_ props comp & args] (reagent/argv this)
                 b (:value props)
                 {os :old-values a :value} @os-ref]
             (let [[_ [old-update :as rest]] (split-with #(not= % b) os)]
               (if (= old-update b)                         ; careful do NOT nil pun
                 (swap! os-ref assoc :old-values (vec rest))
                 (when (not= a b)
                   ; this is either not a big deal, e.g. default values have been applied
                   ; or multiple users are editing the same value and changes are probably being lost
                   (timbre/warn "Potential conflicting concurrent edits detected, discarding local state" {:a a :b b :os os})
                   (reset! os-ref {:old-values [b]
                                   :value b}))))))}))))

(let [on-change (fn [os-ref f n]                            ; letfn not working #470
                  (let [o (last (:old-values @os-ref))]
                    (swap! os-ref (fn [m]
                                    (-> (assoc m :value n)
                                        (update :old-values conj n))))
                    (when f (f o n))))]
  (defn optimistic-updates [props comp & args]
    (let [os-ref (r/atom {:value (:value props)
                          :old-values [(:value props)]})]
      (reagent/create-class
        {:reagent-render
         (fn [props comp & args]
           (let [props (-> props
                           (assoc :value @(r/cursor os-ref [:value]))
                           (update :on-change #(r/partial on-change os-ref %)))]
             (into [comp props] args)))
         :component-did-update
         (fn [this]
           (let [[_ props comp & args] (reagent/argv this)
                 b (:value props)
                 {os :old-values a :value} @os-ref]
             (let [[_ [old-update :as rest]] (split-with #(not= % b) os)]
               (if (= old-update b)                         ; careful do NOT nil pun
                 (swap! os-ref assoc :old-values (vec rest))
                 (when (not= a b)
                   ; this is either not a big deal, e.g. default values have been applied
                   ; or multiple users are editing the same value and changes are probably being lost
                   (timbre/warn "Potential conflicting concurrent edits detected, discarding local state" {:a a :b b :os os})
                   (reset! os-ref {:old-values [b]
                                   :value b}))))))}))))

(let [target-value (fn [e] (.. e -target -value))]          ; letfn not working #470
  (defn textarea [props]
    [:textarea (update-existing props :on-change r/comp target-value)]))

(defn easy-checkbox [props & [label]]
  (let [control [input/checkbox props]]
    (if (blank->nil label)
      [:label (-> props
                  (assoc :style {:font-weight "400"})
                  (dissoc :on-change :checked))
       control " " label]
      control)))

(defn ^:export easy-checkbox-boolean [label r & [props]]
  [easy-checkbox (assoc props
                   :checked @r
                   :on-change (r/partial swap! r not))
   label])

(defn ^:export code [props]
  (let [defaults {:lineNumbers true
                  :matchBrackets true
                  :autoCloseBrackets true
                  :viewportMargin js/Infinity}
        props (into defaults props)]
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
        (update-existing :on-change r/comp read-edn-string))))

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
