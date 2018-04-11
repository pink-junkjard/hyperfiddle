(ns hypercrud.ui.error
  (:require
    [contrib.data :as data]
    [hypercrud.types.Err :as Err]
    [hypercrud.ui.control.markdown-rendered :refer [markdown]]))


(defn e->map [e]
  (cond
    (Err/Err? e) {:message (:msg e)
                  :data (:data e)}
    (map? e) e
    (string? e) {:message e}
    :else {:message (ex-message e)
           :data (ex-data e)
           :cause (ex-cause e)}))

(defn ex-data->human-detail [{:keys [ident human soup] :as data}]
  (or human soup (data/pprint-str data)))

(defn error-inline [e]
  (let [dev-open? true
        {:keys [cause data message]} (e->map e)]
    [:code message " " (if dev-open? (str " -- " (ex-data->human-detail data)))]))

(defn error-block [e]
  (let [dev-open? true
        {:keys [cause data message]} (e->map e)]
    ; todo we don't always return an error with a message
    [:pre
     (if message
       [:h4 message]
       [markdown "#### Unrecognized error (please comment on [#170](https://github.com/hyperfiddle/hyperfiddle/issues/170))"])
     (if dev-open? [:p (ex-data->human-detail data)])
     (if (= :hyperfiddle.error/unrecognized (:ident data))
       [markdown "Please comment this error at [hyperfiddle/170](https://github.com/hyperfiddle/hyperfiddle/issues/170) so we can match it"])]))

(defn error-comp [ctx]
  ; :find-element :attribute :value
  (cond
    (:hypercrud.ui/error ctx) ((:hypercrud.ui/error ctx) ctx)
    (:hypercrud.browser/attribute ctx) error-inline         ; table: header or cell, form: header or cell
    (:hypercrud.browser/find-element ctx) error-inline
    :else error-block))                                     ; browser including inline true links
