(ns recash-upload-console.data-upload.upload-dict-dim
  (:require [recash-upload-console.domain.datomic-utils :as du]
            [recash-upload-console.domain.model :as m]
            [recash-upload-console.domain.db-manager :as dbm]
            [recash-upload-console.common.time-utils :as tu]
            [recash-upload-console.common.utils :as u]
            [recash-upload-console.data-upload.upload-specs :as upl-s]
            [recash-upload-console.data-upload.upload-service :as upl]
            [recash-upload-console.common.open-handlers :as open-handlers]
            [recash-upload-console.common.xml-utils :as xml-u]
            [clojure.spec :as s]
            [datomic.api :as d]
            [recash-upload-console.processings.etl :as etl]
            [recash-upload-console.processings.parsing-and-mapping
             :refer [item->custom->v
                     ignore-mapping?
                     item->field-value
                     item-has-field?
                     map-item-via
                     mapped-item->post
                     v-by-type] :as pnm]
            [recash-upload-console.processings.loading
             :refer [ent->load-item] :as l]
            [recash-upload-console.processings.validating
             :refer [validate-mapped-item]]
            [recash-upload-console.processings.processing :as prcs]
            [clojure.data.json :as json]))

;; -- Testing
(def db-uri
  "datomic:sql://recash-local?jdbc:postgresql://localhost:5432/recash-local?user=datomic&password=datomic")

(defn get-conn
  []
  (dbm/new-conn db-uri))


;; -- ET Side -----------------------------------------------------------------

(defmethod mapped-item->post :check-op-type-dict-dim
  [mapped-item post-kw]
  (cond-> mapped-item
          ;; если удаление, то берем только нужные поля
          (= :D (:dict-dim/op-type mapped-item))
          (select-keys [:dict-dim/op-type
                        :dict-dim/uuid
                        :source/name
                        :source/frgn-uuid
                        :source/frgn-str-id
                        :source/imported-datetime])))


(defmethod validate-mapped-item :dict-dim
  [conn mapped-item mapped-item-type]
  (if (s/valid? ::recash-upload-console.data-upload.upload-specs/dict-dim mapped-item)
   []
   [(s/explain-str ::recash-upload-console.data-upload.upload-specs/dict-dim mapped-item)]))


;; -- LOAD side ---------------------------------------------------------------
(defmethod ent->load-item [:dict-dim :datomic-tx]
  [conn mapped-item mapped-item-type load-item-type]
  (let [e mapped-item]
    (println "ent->load-item")
    (println e)
    (if (= :D (:dict-dim/op-type e))
      ;; при удалении
      (if-let [e-to-delete (m/e-of-foreign-dimension conn e)]
        [[:db/retractEntity e-to-delete]]
        ;; TODO: стоит ли выкидывать исключение или просто игнорить?
        (do
          (println "IGNORE because couldn't find dim to delete: " e)
          :ignore))
        ; (throw (Exception. "Не найдено измерение для удаления: " e)))
      ;; если новое или редактирование
      (if-let [dim-group-eid (when (:dict-dim/group-name e)
                               (m/dim-group-by-name conn (:dict-dim/group-name e)))]
        (-> (u/info-map m
              ;; транзакция по измерению
              :dim-tx (-> {:dimension/name (:dict-dim/name e)
                           :dimension/editable? (:dict-dim/editable? e)
                           :dimension/group (:db/id dim-group-eid)
                           :source/imported-datetime (tu/now-jdate)}
                          (merge (select-keys e [:source/name :source/frgn-uuid :source/frgn-str-id]))
                          (dissoc :op-type)
                          ;; если уже есть в базе - значит редактирование, иначе добавление
                          (#(if-let [exist-e (m/e-of-foreign-dimension conn e)]
                               (assoc % :db/id (:db/id exist-e))
                               (assoc % :dimension/uuid (d/squuid))))
                          (u/remove-nil-keys)))
            :dim-tx
            vector)
        (throw (Exception. (str "Не найдена группа измерений для добавления нового измерения"
                                e)))))))




;; -- TESTING -----------------------------------------------------------------
; (ent->load-item (get-conn)
;                 {:dict-dim/op-type :U
;                  :dict-dim/group-name "Контрагенты"
;                  :dict-dim/editable? false
;                  :dict-dim/name "Новый клиент"
;                  :source/name "ummastore"
;                  :source/frgn-str-id "client5345"}
;                 :dict-dim
;                 :datomic-tx)
;
; (def test-process-params
;  {:id   :processing-json
;   :name "Обработка json от ummastore"
;   :stages
;     [{:id :stage-etl-dict-dims
;       :name "ETL справочников с json"
;       :run-when :always
;       :action
;        {:action-type :etl-by-items
;         ; :debug-mode? true
;         :load-params
;          {:type :datomic-tx}
;         :pnm-params
;          {:parse {:from :json
;                   :item-params {:tags :catalogs}}
;           :mapping
;            {:to :dict-dim
;             :post-mapping :check-op-type-dict-dim
;             :via {:method :mpvectors
;                   :params
;                    {:source/name         [:const "ummastore"]
;                     :source/frgn-str-id  [:field :id :str]
;                     :dict-dim/op-type    [:field :opType :match {:ms {"U" :U  "D" :D}}]
;                     :dict-dim/name       [:field :name :str {:opt? true}]
;                     :dict-dim/group-name [:field :catName :str {:opt? true}]}}}}}}
;      {:id :stage-etl-entries
;       :name "ETL самих записей с json"
;       :run-when [:stage-etl-dict-dims]
;       :action
;        {:action-type :etl-by-items
;         ; :debug-mode? true
;         :load-params
;           {:type :datomic-tx}
;          :pnm-params
;           {:parse {:from :json
;                    :item-params {:tags :entries}}
;            :mapping
;             {:to :st-entry
;              :post-mapping :check-op-type-st-entry
;              :via {:method :mpvectors
;                    :params
;                     {:source/name        [:const "ummastore"]
;                      :source/frgn-str-id [:field :id :str]
;                      :st-entry/op-type   [:field :opType :match {:ms {"U" :U  "D" :D}}]
;                      :st-entry/date      [:field :date :datetime {:opt? true}]
;                      :st-entry/summ      [:field :summ :double {:opt? true}]
;                      :st-entry/v-flow    [:field :flowType :match {:opt? true :ms {1 :inflow 0 :outflow}}]
;                      :st-entry/v-type    [:const :fact]
;                      :st-entry/editable? [:const false]
;                      :st-entry/dims      [:custom
;                                           :st-entry-dims
;                                           {:opt? true ; часть измерений может отсутствовать в entry
;                                            :defaults {:source/name        [:const "ummastore"]
;                                                       :source/frgn-str-id [:field :_this :str]
;                                                       :st-dim/type        [:const :must-pre-exist]
;                                                       :st-dim/editable?   [:const false]}
;                                            :dims {:client  {:st-dim/group-name [:const "Клиенты"]}
;                                                   :store   {:st-dim/group-name [:const "Магазины"]}
;                                                   :account {:st-dim/group-name [:const "Счета BRK"]}}}]}}}}}}]})
;
; (def test-json-source (slurp "import_files/json-example.json"))
;
;
;
;
; (defn t-st1
;   []
;   (let [conn (get-conn)
;         source test-json-source
;         stage-params (get-in test-process-params [:stages 0])]
;     (prcs/process-stage-with-logs conn
;                                   source
;                                   stage-params)))
;
; (defn t-st2
;   []
;   (let [conn (get-conn)
;         source test-json-source
;         stage-params (get-in test-process-params [:stages 1])]
;     (prcs/process-stage-with-logs conn
;                                   source
;                                   stage-params)))
