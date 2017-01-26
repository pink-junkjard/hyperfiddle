(ns hypercrud.ui.table
  (:require [clojure.string :as string]
            [hypercrud.browser.links :as links]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.platform.native-event-listener :refer [native-listener]] ;provided dependency
            [hypercrud.types :refer [->DbId ->DbVal]]
            [hypercrud.ui.auto-control :refer [auto-table-cell connection-color]]
            [hypercrud.ui.renderer :as renderer]
            [hypercrud.util :as util]
            [reagent.core :as r]))


(defn sortable? [field]
  (let [{:keys [:attribute/cardinality :attribute/valueType]} (:field/attribute field)]
    (and
      (= (:db/ident cardinality) :db.cardinality/one)
      ; ref requires more work (inspect label-prop)
      (contains? #{:db.type/keyword
                   :db.type/string
                   :db.type/boolean
                   :db.type/long
                   :db.type/bigint
                   :db.type/float
                   :db.type/double
                   :db.type/bigdec
                   :db.type/instant
                   :db.type/uuid
                   :db.type/uri
                   :db.type/bytes
                   :db.type/code}
                 (:db/ident valueType)))))


(defn build-col-heads [ordered-find-elements col-sort]
  (let [[form-dbid' ident' direction] @col-sort]
    (->> ordered-find-elements
         (mapv :find-element/form)
         (mapcat (fn [{form-dbid :db/id fields :form/field :as form}]
                   (->> fields
                        (sort-by :field/order)
                        (mapv (juxt (constantly form-dbid) identity)))))
         (map (fn [[form-dbid {:keys [:field/prompt] :as field}]]
                (let [ident (-> field :field/attribute :attribute/ident)
                      with-sort-direction (fn [asc desc no-sort not-sortable]
                                            (if (sortable? field)
                                              (if (and (= form-dbid' form-dbid) (= ident' ident))
                                                (condp = direction
                                                  :asc asc
                                                  :desc desc)
                                                no-sort)
                                              not-sortable))
                      on-click (with-sort-direction #(reset! col-sort [form-dbid ident :desc])
                                                    #(reset! col-sort nil)
                                                    #(reset! col-sort [form-dbid ident :asc])
                                                    (constantly nil))
                      arrow (with-sort-direction " ↓" " ↑" " ↕" nil)
                      css-encode (fn [s]
                                   ;todo flesh this out
                                   ; http://stackoverflow.com/a/449000/959627
                                   (-> s
                                       (string/replace ":" "_co_")
                                       (string/replace "/" "_bs_")
                                       (string/replace " " "_sp_")))
                      css-class (->> [(str "field-id-" (get-in field [:db/id :id]))
                                      (some->> (:field/prompt field) (str "field-prompt-"))
                                      (some->> field :field/attribute :attribute/ident (str "field-attr-"))]
                                     (remove nil?)
                                     (map css-encode)
                                     (interpose " ")
                                     (apply str))]
                  [:td {:class css-class :key (:db/id field) :on-click on-click}
                   prompt
                   (let [docstring (-> field :field/attribute :attribute/doc)]
                     (if-not (empty? docstring)
                       [native-listener {:on-click (fn [e]
                                                     (js/alert docstring)
                                                     (.stopPropagation e))}
                        [:span.help "ⓘ"]]))
                   [:span.sort-arrow arrow]]))))))


(defn build-row-cells [form entity repeating-link-ctxs {:keys [super-graph] :as param-ctx}]
  (let [link-fn (let [link-ctx-by-ident (->> repeating-link-ctxs
                                             (mapv (juxt #(-> % :link-ctx/ident) identity))
                                             (into {}))]
                  (fn [ident label param-ctx]
                    (let [link-ctx (get link-ctx-by-ident ident)
                          props (links/build-link-props link-ctx param-ctx)]
                      [(:navigate-cmp param-ctx) props label param-ctx])))
        param-ctx (assoc param-ctx :color ((:color-fn param-ctx) entity param-ctx)
                                   :owner ((:owner-fn param-ctx) entity param-ctx))
        style {:border-color (connection-color (:color param-ctx))}]
    (->> (:form/field form)
         (sort-by :field/order)
         (map (fn [field]
                [:td.truncate {:key (:db/id field) :style style}
                 (if-let [renderer (renderer/renderer-for-attribute (:field/attribute field))]
                   (let [{renderer :value error :error} (eval renderer)]
                     [:div.value
                      (if error
                        (pr-str error)
                        (try
                          (renderer super-graph link-fn entity)
                          (catch :default e (pr-str e))))])
                   (let [link-ctxs (filter #(= (.-dbid field) (some-> % :link-ctx/field :db/id)) repeating-link-ctxs)]
                     [auto-table-cell entity field link-ctxs param-ctx]))])))))


;(defn links-cell [entity form repeating-links retract-entity! show-links? navigate-cmp]
;  (let [open? (r/atom false)]
;    (fn [entity form retract-entity! show-links? navigate-cmp]
;      (if @open?
;        [:div.link-menu
;         (if show-links?
;           (conj
;             (->> repeating-links
;                  (map (fn [{:keys [:link/ident :link/prompt] :as link}]
;                         (let [param-ctx {:user-profile hc/*user-profile*
;                                          :result ???}]
;                           (links/query-link link param-ctx #(navigate-cmp {:key ident :href %} prompt))))))
;             (if retract-entity!
;               [:span {:key "hypercrud-delete-row" :on-click #(retract-entity! (:db/id entity))} "Delete Row"])))
;         [:span {:key "close" :on-click #(reset! open? false)} "Close"]]
;        [:div {:on-click #(reset! open? true)} "⚙"]))))


(defn table-row [result ordered-find-elements repeating-link-ctxs param-ctx]
  (let [param-ctx (assoc param-ctx :result result)]
    [:tr
     (mapcat (fn [find-element]
               (let [form (:find-element/form find-element)
                     entity (get result (:find-element/name find-element))]
                 (build-row-cells form entity repeating-link-ctxs param-ctx)))
             ordered-find-elements)
     [:td.link-cell {:key :link-cell}
      (->> repeating-link-ctxs
           (filter #(nil? (:link-ctx/field %)))
           (filter #(links/link-visible? % param-ctx))
           (map (fn [link-ctx]
                  (let [props (assoc (links/build-link-props link-ctx param-ctx) :key (:db/id link-ctx))]
                    ((:navigate-cmp param-ctx) props (-> link-ctx :link-ctx/link :link/prompt) param-ctx))))
           (interpose " · "))]]))


(defn body [resultset ordered-find-elements repeating-link-ctxs sort-col param-ctx]
  [:tbody
   (let [[form-dbid sort-key direction] @sort-col
         sort-eids (fn [resultset]
                     (let [{form :find-element/form find-element-name :find-element/name}
                           (->> ordered-find-elements
                                (filter #(= form-dbid (-> % :find-element/form :db/id)))
                                first)
                           field (->> (:form/field form)
                                      (filter #(= sort-key (-> % :field/attribute :attribute/ident)))
                                      first)]
                       (if (and (not= nil field) (sortable? field))
                         (sort-by #(get-in % [find-element-name sort-key])
                                  (condp = direction
                                    :asc #(compare %1 %2)
                                    :desc #(compare %2 %1))
                                  resultset)
                         resultset)))]
     (->> resultset
          sort-eids
          (map (fn [result]
                 ^{:key (hash (util/map-values :db/id result))}
                 [table-row result ordered-find-elements repeating-link-ctxs param-ctx]))))])


(defn table [resultset ordered-find-elements link-ctxs param-ctx]
  (let [sort-col (r/atom nil)]
    (fn [resultset ordered-find-elements link-ctxs param-ctx]
      (let [repeating-link-ctxs (filter :link-ctx/repeating? link-ctxs)]
        [:table.ui-table
         [:thead
          [:tr
           (build-col-heads ordered-find-elements sort-col)
           [:td.link-cell {:key :link-cell}
            (->> (remove :link-ctx/repeating? link-ctxs)
                 (filter #(nil? (:link-ctx/field %)))
                 (filter #(links/link-visible? % param-ctx))
                 (map (fn [{:keys [:link-ctx/link] :as link-ctx}]
                        (let [props (links/build-link-props link-ctx param-ctx)]
                          ^{:key (:db/id link-ctx)}
                          [(:navigate-cmp param-ctx) props (:link/prompt link) param-ctx])))
                 (interpose " · "))]]]
         [body resultset ordered-find-elements repeating-link-ctxs sort-col param-ctx]]))))
