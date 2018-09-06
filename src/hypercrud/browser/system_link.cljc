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
   (let [{:keys [::field/path-segment]} field
         path (or (some->> path-segment (conj path)) path)  ; fe wrapping causes spaces in paths
         ?spath (blank->nil (string/join " " path))]
     (-> (->> (::field/children field)
              #_(filter ::field/data-has-id?)
              (mapcat (fn [child-field]
                        (nested-links-for-field parent-fiddle dbname schema child-field path (::field/data-has-id? field)))))
         (cond->>
           (and #_(::field/data-has-id? field)
                (not= '* path-segment))
           (cons {:db/id (keyword "hyperfiddle.browser.system-link" (str "remove-" (hash path)))
                  :hypercrud/sys? true
                  :link/rel (if (#{:db/id :db/ident} path-segment) :hf/remove :hf/detach)
                  :link/class #{path-segment}
                  :link/path ?spath})

           (and #_(::field/data-has-id? field)
                (or (or (not (nil? path-segment))
                        (not= :entity (:fiddle/type parent-fiddle)))
                    (and (context/attribute-segment? path-segment)
                         (not= '* path-segment))))
           (cons {:db/id (keyword "hyperfiddle.browser.system-link" (str "edit-" (hash path)))
                  :hypercrud/sys? true
                  :link/rel :hf/edit
                  :link/class #{path-segment}
                  :link/path ?spath
                  :link/fiddle (system-fiddle/fiddle-system-edit dbname)})

           true #_parent-has-id?
           (cons {:db/id (keyword "hyperfiddle.browser.system-link" (str "new-" (hash path)))
                  :hypercrud/sys? true
                  ;:link/disabled? (context/attribute-segment? (::field/path-segment field))
                  :link/rel :hf/new
                  :link/class #{path-segment}
                  :link/path ?spath}))))))

(defn- system-links-impl [parent-fiddle fields schemas]     ; always the top - the root links, never parent-child
  (->> fields
       (filter ::field/source-symbol)
       (mapcat (fn [{:keys [::field/path-segment] :as field}]
                 (let [dbname (str (::field/source-symbol field))
                       schema (get schemas dbname)]
                   (cond->> (nested-links-for-field parent-fiddle dbname schema field [] false)
                     (not= :entity (:fiddle/type parent-fiddle))
                     (cons {:db/id (keyword "hyperfiddle.browser.system-link" (str "new-" (hash path-segment)))
                            :hypercrud/sys? true
                            :link/rel :hf/new
                            :link/class (if path-segment #{})
                            :link/path (blank->nil (str path-segment)) ; nil path means `:find (count ?x) .`
                            :link/fiddle (system-fiddle/fiddle-system-edit dbname)})))))))

(defn console-links
  "All sys links can be matched and merged with user-links. Matching is determined by link/rel and link/path"
  [parent-fiddle field schemas]
  (if (::field/source-symbol field)                         ; top level never has one, recursive always does
    (system-links-impl parent-fiddle [field] schemas)       ; karl was lazy and never untangled the fe wrapping
    (system-links-impl parent-fiddle (::field/children field) schemas)))
