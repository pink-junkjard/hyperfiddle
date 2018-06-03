(ns hypercrud.ui.error
  (:require
    [contrib.pprint :refer [pprint-str]]
    [hypercrud.types.Err :as Err]
    [hyperfiddle.ui :refer [markdown]]
    [hyperfiddle.foundation :as foundation]))


(defn e->map [e]
  (cond
    (Err/Err? e) {:message (:msg e)
                  :data (:data e)}
    (map? e) e
    (string? e) {:message e}
    :else {:message (ex-message e)
           :data (ex-data e)
           :cause (ex-cause e)}))

(defn ex-data->human-detail [{:keys [ident error-msg] :as data}]
  (or error-msg (pprint-str data)))

(defn error-inline [e]
  (let [{:keys [cause data message]} (e->map e)]
    (str message #_#_" " (str " -- " (ex-data->human-detail data)))))

(defn error-block [e]
  (let [{:keys [cause data message]} (e->map e)]            ; we don't always return an error with a message
    [:pre
     [:h3 message]
     [markdown (str "```\n" (ex-data->human-detail data) "\n```\n")]
     (if (:human-hint data) [markdown (:human-hint data)])
     #_(if (:query data) [markdown (str "```\n" (:query data) "\n```")])]))

(defn error-comp [ctx]
  ; :find-element :attribute :value
  (cond
    (:hypercrud.ui/error ctx) ((:hypercrud.ui/error ctx) ctx)
    (:hypercrud.browser/attribute ctx) error-inline         ; table: header or cell, form: header or cell
    (:hypercrud.browser/find-element ctx) error-inline
    ; browser including inline true links
    :else (fn [e]
            [:div
             [error-block e]
             #_(if (some-> e e->map :data :ident (= :db.error/datoms-conflict)))
             [foundation/staging (:peer ctx)]])))
