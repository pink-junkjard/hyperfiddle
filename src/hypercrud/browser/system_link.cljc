(ns hypercrud.browser.system-link
  (:require
    [clojure.string :as string]
    [hypercrud.browser.find-element :as field]
    [hypercrud.browser.system-fiddle :as system-fiddle]))


(defn ^:export system-link? [link-id]
  (and (keyword? link-id)
       (some-> (namespace link-id) (string/starts-with? "hyperfiddle.browser.system-link"))))


(def retract-formula
  "(fn [ctx multi-color-tx modal-route]
  {:tx {(:uri ctx) [[:db.fn/retractEntity @(contrib.reactive/cursor (:hypercrud.browser/data ctx) [:db/id])]]}})")

(defn links-for-field [dbname path field parent-has-id?]
  (let [path (conj path (::field/path-segment field))
        s-path (string/join " " path)]
    (-> (->> (::field/children field)
             (filter ::field/data-has-id?)
             (map #(links-for-field dbname path % (::field/data-has-id? field))))
        (cond->>
          (and (::field/path-segment field) #_(= :db.cardinality/one (::field/cardinality)))
          (cons {:db/id (keyword "hyperfiddle.browser.system-link" (str "edit-" (hash path)))
                 :hypercrud/sys? true
                 ; todo disable when entity link AND root field
                 ;:link/disabled? true
                 :link/rel :hyperfiddle/edit
                 :link/path s-path
                 ;:link/dependent? true
                 :link/fiddle (system-fiddle/fiddle-system-edit dbname)
                 :link/managed? false})

          parent-has-id?
          (cons {:db/id (keyword "hyperfiddle.browser.system-link" (str "new-" (hash path)))
                 :hypercrud/sys? true
                 ; todo disable when entity link AND root field
                 ;:link/disabled? true
                 :link/rel :hyperfiddle/new
                 :link/path s-path                          ; todo need to know if there is a parent/child relationship to manage
                 ;:link/dependent? false      ; not managed, no parent-child ref
                 ;:link/dependent? true       ; manged - need parent-child ref
                 :link/render-inline? true
                 :link/fiddle (system-fiddle/fiddle-system-edit dbname)
                 :link/create? true
                 :link/managed? true})

          ; why is the parent's dbid necessary to retract an entity?
          (and parent-has-id? #_(= :db.cardinality/one (::field/cardinality)))
          (cons {:db/id (keyword "hyperfiddle.browser.system-link" (str "remove-" (hash path)))
                 :hypercrud/sys? true
                 ;:link/disabled? true
                 :link/rel :hyperfiddle/remove
                 :link/path s-path
                 ;:link/dependent? true
                 :link/render-inline? true
                 :link/fiddle system-fiddle/fiddle-blank-system-remove
                 :link/managed? true
                 :link/tx-fn retract-formula})))))

(defn system-links
  "All sys links can be matched and merged with user-links. Matching is determined by link/rel and link/path"
  [fields schemas]
  ; todo independent new link (for query)
  (->> fields
       (filter ::field/source-symbol)
       (map (fn [field]
              (let [dbname (str (::field/source-symbol field))]
                (links-for-field dbname [:body] field (::field/data-has-id? field)))))))
