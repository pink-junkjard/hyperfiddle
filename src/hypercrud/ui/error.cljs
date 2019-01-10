(ns hypercrud.ui.error
  (:require
    [contrib.pprint :refer [pprint-str]]
    [contrib.reactive :as r]
    [contrib.ui :refer [markdown]]
    [hypercrud.types.Err :as Err]
    [hyperfiddle.ide.staging :refer [staging]]))


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

(defn error-inline [e & [props]]
  (let [{:keys [cause data message]} (e->map e)]
    [:span props
     (str message #_#_" " (str " -- " (ex-data->human-detail data)))]))

(defn error-block [e & [props]]
  (let [{:keys [cause data message]} (e->map e)]            ; we don't always return an error with a message
    [:div props
     [:h3 message]
     [markdown (str "```\n" (ex-data->human-detail data) "\n```\n")]
     (if (:human-hint data) [markdown (:human-hint data)])
     #_(if (:query data) [markdown (str "```\n" (:query data) "\n```")])]))

(defn error-block-with-stage [ctx e & [props]]
  [:<>
   [error-block e props]
   (when (:branch ctx)
     [staging ctx])])

(defn error-comp [ctx]
  ; :find-element :attribute :value
  (cond
    (:hypercrud.ui/error ctx) ((:hypercrud.ui/error ctx) ctx)

    (> (count (:hypercrud.browser/path ctx)) 0) error-inline

    ; browser including inline true links
    :else (r/partial error-block-with-stage ctx)))
