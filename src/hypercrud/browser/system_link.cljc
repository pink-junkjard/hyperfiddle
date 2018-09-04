(ns hypercrud.browser.system-link
  (:require
    [clojure.string :as string]
    [contrib.string :refer [blank->nil]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.system-fiddle :as system-fiddle]))


(defn ^:export system-link? [link-id]
  (and (keyword? link-id)
       (some-> (namespace link-id) (string/starts-with? "hyperfiddle.browser.system-link"))))

(defn nested-links-for-field
  ([parent-fiddle dbname schema field path parent-has-id?]
   (let [path (or (some->> (::field/path-segment field) (conj path)) path) ; fe wrapping causes spaces in paths
         ?spath (blank->nil (string/join " " path))]
     (-> (->> (::field/children field)
              (filter ::field/data-has-id?)
              (mapcat (fn [child-field]
                        (nested-links-for-field parent-fiddle dbname schema child-field path (::field/data-has-id? field)))))
         (cond->>
           (and (::field/data-has-id? field)
                (not= '* (::field/path-segment field)))
           (cons {:db/id (keyword "hyperfiddle.browser.system-link" (str "remove-" (hash path)))
                  :hypercrud/sys? true
                  :link/disabled? (context/attribute-segment? (::field/path-segment field))
                  :link/rel :hf/remove
                  :link/path ?spath})

           (and (::field/data-has-id? field)
                (or (or (not (nil? (::field/path-segment field)))
                        (not= :entity (:fiddle/type parent-fiddle)))
                    (and (context/attribute-segment? (::field/path-segment field))
                         (not= '* (::field/path-segment field)))))
           (cons {:db/id (keyword "hyperfiddle.browser.system-link" (str "edit-" (hash path)))
                  :hypercrud/sys? true
                  ;:link/disabled? (context/attribute-segment? (::field/path-segment field))
                  :link/rel :hf/edit
                  :link/path ?spath
                  :link/fiddle (system-fiddle/fiddle-system-edit dbname)})

           parent-has-id?
           (cons {:db/id (keyword "hyperfiddle.browser.system-link" (str "new-" (hash path)))
                  :hypercrud/sys? true
                  ;:link/disabled? (context/attribute-segment? (::field/path-segment field))
                  :link/rel :hf/new
                  :link/path ?spath}))))))

(defn- system-links-impl [parent-fiddle fields schemas]     ; always the top - the root links, never parent-child
  (->> fields
       (filter ::field/source-symbol)
       (mapcat (fn [field]
                 (let [dbname (str (::field/source-symbol field))
                       schema (get schemas dbname)]
                   (cond->> (nested-links-for-field parent-fiddle dbname schema field [] false)
                     (not= :entity (:fiddle/type parent-fiddle))
                     (cons (let [path (::field/path-segment field)]
                             ; nil path means `:find (count ?x) .`
                             {:db/id (keyword "hyperfiddle.browser.system-link" (str "new-" (hash path)))
                              :hypercrud/sys? true
                              :link/rel :hf/new
                              :link/path (blank->nil (str path))
                              :link/fiddle (system-fiddle/fiddle-system-edit dbname)}))))))))

(defn console-links
  "All sys links can be matched and merged with user-links. Matching is determined by link/rel and link/path"
  [parent-fiddle field schemas]
  (if (::field/source-symbol field)                         ; top level never has one, recursive always does
    (system-links-impl parent-fiddle [field] schemas)       ; karl was lazy and never untangled the fe wrapping
    (system-links-impl parent-fiddle (::field/children field) schemas)))
