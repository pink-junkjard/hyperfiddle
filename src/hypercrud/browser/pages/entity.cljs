(ns hypercrud.browser.pages.entity
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<! >! chan]]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx-util :as tx-util]
            [hypercrud.ui.form :refer [cj-form]]
            [promesa.core :as p]))


(defn view [cur client forms cmd-chan eid]
  "hypercrud values just get a form, with ::update and ::delete."
  (let [eid (js/parseInt eid 10)
        local-datoms (cur [:form] [])
        client (hc/with client @local-datoms)
        local-transact! (fn [more-datoms]
                          (swap! local-datoms (fn [old-datoms]
                                                (vec (tx-util/normalize-tx old-datoms more-datoms)))))]
    [:div
     [cj-form client eid forms local-transact!]
     (if (tx-util/tempid? eid)
       ;[:button {:on-click #(go (>! cmd-chan [::create-item client href @form-cur]))} "Create"]
       nil
       [:button {:on-click #(go (>! cmd-chan [::update-item client @local-datoms]))}
        "Update"])]))


;; controller actions, are these "Commands" ?
;; can a command execute more commands? I think no
;; does a command have write access to global app state? i think yes,
;; for example, a command might trigger a page navigation.
;; does a command have access to encapsulated state? E.g. a component
;; might be on the page multiple times, and a command could change it's state.
;; So, maybe two types of commands, one that has encapsulation, and one that doesn't?

(def commands
  {::update-item
   (fn [client datoms]
     (->> (hc/transact! client datoms)
          (p/map (fn [resp]
                   (if (:success resp)
                     (js/alert "ok")
                     (js/alert "error"))))))

   ;::create-item
   #_(fn [client href cj-form]                              ; it would be great if the href was deducible from the cj-form
       (let [body {:template (-> cj-form :data)}]
         (->> (hypercrud/create client href body)
              (fmap (fn [resp]
                      (if (:success resp)
                        (js/alert "ok")
                        (js/alert "error")))))))})

