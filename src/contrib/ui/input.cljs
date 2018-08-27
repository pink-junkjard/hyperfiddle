(ns contrib.ui.input
  (:refer-clojure :exclude [boolean keyword long])
  (:require
    [contrib.css :refer [css]]
    [contrib.data :refer [update-existing]]
    [contrib.reactive :as r]
    [contrib.reader]
    [contrib.string :refer [blank->nil]]
    [reagent.core :as reagent]))


(let [checked (fn [e] (.. e -target -checked))]             ; letfn not working #470
  (defn checkbox [props]
    [:input (-> (assoc props :type "checkbox")
                (update-existing :on-change r/comp checked))]))

(let [target-value (fn [e] (.. e -target -value))]          ; letfn not working #470
  (defn text [props]
    [:input (-> (assoc props :type "text")
                (update-existing :on-change r/comp target-value))]))

(let [on-change (fn [state parse-string new-s-value]
                  (swap! state assoc :s-value new-s-value)
                  (let [new-value (parse-string new-s-value)] ; todo this should be atomic, but we still want to throw
                    (swap! state assoc :last-valid-value new-value)
                    new-value))
      initial-state-val (fn [to-string props]
                          {:s-value (to-string (:value props))
                           :last-valid-value (:value props)})]
  (defn text-adapter [parse-string to-string props]
    (let [state (r/atom (initial-state-val to-string props))]
      (reagent/create-class
        {:reagent-render
         (fn [parse-string to-string props]
           (let [s-value @(r/cursor state [:s-value])]
             [text (-> (assoc props :value s-value)
                       (update :class css (let [valid? (try (parse-string s-value) true
                                                            (catch :default e false))]
                                            (when-not valid? "invalid")))
                       (update :on-change (fn [f]
                                            (r/comp
                                              (or f identity)
                                              (r/partial on-change state parse-string)))))]))
         :component-did-update
         (fn [this]
           (let [[_ parse-string to-string props] (reagent/argv this)]
             (when-not (= (:last-valid-value @state) (:value props))
               (reset! state (initial-state-val to-string props)))))}))))

(let [parse-string (fn [s]                                  ; letfn not working #470
                     (let [v (some-> s contrib.reader/read-edn-string)]
                       (assert (or (nil? v) (keyword? v)))
                       v))
      to-string (fn [v] (some-> v pr-str))]
  (defn keyword [props]
    [text-adapter parse-string to-string props]))

(let [parse-string (fn [s] (some-> s contrib.reader/read-edn-string)) ; letfn not working #470
      to-string (fn [v] (some-> v pr-str))]
  (defn edn [props]
    [text-adapter parse-string to-string props]))

(let [parse-string (fn [s]                                  ; letfn not working #470
                     (when-let [s (blank->nil s)]
                       (let [v (js/parseInt s 10)]
                         (assert (integer? v))
                         v)))]
  (defn long [props]
    [text-adapter parse-string str props]))
