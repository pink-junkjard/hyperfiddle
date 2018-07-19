(ns hypercrud.browser.system-link
  (:require
    [clojure.string :as string]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.system-fiddle :as system-fiddle]))


(defn ^:export system-link? [link-id]
  (and (keyword? link-id)
       (some-> (namespace link-id) (string/starts-with? "hyperfiddle.browser.system-link"))))

(def retract-formula
  "(fn [ctx multi-color-tx modal-route]
  {:tx {(:uri ctx) [[:db.fn/retractEntity @(contrib.reactive/cursor (:hypercrud.browser/data ctx) [:db/id])]]}})")

(defn body-links-for-field
  ([parent-fiddle dbname schema field]
   (body-links-for-field parent-fiddle dbname schema field [:body] false))
  ([parent-fiddle dbname schema field path parent-has-id?]
   (let [path (conj path (::field/path-segment field))
         s-path (string/join " " path)]
     (-> (->> (::field/children field)
              (filter ::field/data-has-id?)
              (mapcat (fn [child-field]
                        (let [path (if (context/attribute-segment? (::field/path-segment field))
                                     (conj path :body)
                                     path)]
                          (body-links-for-field parent-fiddle dbname schema child-field path (::field/data-has-id? field))))))
         (cond->>
           (and (::field/data-has-id? field)
                (or (context/find-element-segment? (::field/path-segment field))
                    (and (context/attribute-segment? (::field/path-segment field))
                         (not= '* (::field/path-segment field)))))
           (cons (let [path (if (= :db.cardinality/many (get-in schema [(::field/path-segment field) :db/cardinality :db/ident]))
                              (conj path :body)
                              path)
                       s-path (string/join " " path)]
                   {:db/id (keyword "hyperfiddle.browser.system-link" (str "remove-" (hash path)))
                    :hypercrud/sys? true
                    :link/disabled? (context/attribute-segment? (::field/path-segment field))
                    :link/rel :hyperfiddle/remove
                    :link/path s-path
                    :link/render-inline? true
                    :link/fiddle system-fiddle/fiddle-blank-system-remove
                    :link/managed? true
                    :link/tx-fn retract-formula}))

           (or (and (context/find-element-segment? (::field/path-segment field))
                    (not= :entity (:fiddle/type parent-fiddle)))
               (and (context/attribute-segment? (::field/path-segment field))
                    (not= '* (::field/path-segment field))))
           (cons (let [path (if (= :db.cardinality/many (get-in schema [(::field/path-segment field) :db/cardinality :db/ident]))
                              (conj path :body)
                              path)
                       s-path (string/join " " path)]
                   {:db/id (keyword "hyperfiddle.browser.system-link" (str "edit-" (hash path)))
                    :hypercrud/sys? true
                    :link/disabled? (context/attribute-segment? (::field/path-segment field))
                    :link/rel :hyperfiddle/edit
                    :link/path s-path
                    :link/fiddle (system-fiddle/fiddle-system-edit dbname)
                    :link/managed? false}))

           parent-has-id?
           (cons {:db/id (keyword "hyperfiddle.browser.system-link" (str "new-" (hash path)))
                  :hypercrud/sys? true
                  :link/disabled? (context/attribute-segment? (::field/path-segment field))
                  :link/rel :hyperfiddle/new
                  :link/path s-path
                  :link/render-inline? true
                  :link/fiddle (system-fiddle/fiddle-system-edit dbname)
                  :link/create? true
                  :link/managed? true}))))))

(defn system-links
  "All sys links can be matched and merged with user-links. Matching is determined by link/rel and link/path"
  [parent-fiddle fields schemas]
  (->> fields
       (filter ::field/source-symbol)
       (mapcat (fn [field]
                 (let [dbname (str (::field/source-symbol field))
                       schema (get schemas dbname)]
                   (cond->> (body-links-for-field parent-fiddle dbname schema field)
                     (not= :entity (:fiddle/type parent-fiddle))
                     (cons (let [path [:head (::field/path-segment field)]
                                 s-path (string/join " " path)]
                             {:db/id (keyword "hyperfiddle.browser.system-link" (str "new-" (hash path)))
                              :hypercrud/sys? true
                              :link/rel :hyperfiddle/new
                              :link/path s-path
                              :link/render-inline? true
                              :link/fiddle (system-fiddle/fiddle-system-edit dbname)
                              :link/create? true
                              :link/managed? true}))))))))
