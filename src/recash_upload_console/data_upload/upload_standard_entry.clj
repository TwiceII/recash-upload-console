(ns recash-upload-console.data-upload.upload-standard-entry
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
            [recash-upload-console.processings.processing :as prcs]))

;; -- Testing
(def db-uri
  "datomic:sql://recash-local?jdbc:postgresql://localhost:5432/recash-local?user=datomic&password=datomic")

(defn get-conn
  []
  (dbm/new-conn db-uri))


;; -- ET Side -----------------------------------------------------------------
(defmethod ignore-mapping? :entry-date-before-2017?
  [item pred-key]
  (if-not (= "D" (:Operation item))
    (let [date (v-by-type (:Date item) {} :date)]
      (tu/jdates-before? date (tu/jdate 2017 1 1)))
    false))


(defmethod mapped-item->post :check-op-type-st-entry
  [mapped-item post-kw]
  (cond-> mapped-item
          ;; если удаление, то берем только нужные поля
          (= :D (:st-entry/op-type mapped-item))
          (select-keys [:st-entry/op-type
                        :st-entry/uuid
                        :source/name
                        :source/frgn-uuid
                        :source/frgn-str-id
                        :source/imported-datetime])))


(defmethod item->custom->v :st-entry-dims
  [item custom-params custom-type item-parsed-from]
  (reduce-kv (fn [result-dims dk d-mapping]
               (if-not (item-has-field? item dk item-parsed-from)
                 (when-not (:opt? custom-params)
                   (throw (Exception. (str "Not found field: " dk " in item: " item))))
                 (let [current-dim-item (item->field-value item dk item-parsed-from)
                       new-st-dim (when-not (u/nil-or-empty-or-blank? current-dim-item)
                                    (-> current-dim-item
                                        (map-item-via {:method :mpvectors
                                                       :params (:defaults custom-params)}
                                                      item-parsed-from)
                                        (merge (map-item-via current-dim-item
                                                             {:method :mpvectors
                                                              :params d-mapping}
                                                             item-parsed-from))))]
                   (if new-st-dim
                     (conj result-dims new-st-dim)
                     result-dims))))
             [] (:dims custom-params)))


(defmethod validate-mapped-item :st-entry
  [conn mapped-item mapped-item-type]
  (if (s/valid? ::recash-upload-console.data-upload.upload-specs/st-entry mapped-item)
   []
   [(s/explain-str ::recash-upload-console.data-upload.upload-specs/st-entry mapped-item)]))


;; -- LOAD Side ---------------------------------------------------------------

(defn temp-id-for-st-dim
  "Получить временный id для измерения"
  [dim]
  (str (:source/name dim)
       "_"
       (or (:source/frgn-uuid dim)
           (:source/frgn-str-id dim)
           (:st-dim/name dim))))


(defn new-st-dim-txs
  "Получить датомы для добавления нового стандартного измерения"
  [conn dim]
  (let [group-name (:st-dim/group-name dim)]
    (if-let [dg-eid (:db/id (du/attr-val->e-single conn :dim-group/name group-name))]
      (if-let [new-dim-name (:st-dim/name dim)]
        (-> {:dimension/uuid (d/squuid)
             :dimension/name new-dim-name
             :dimension/editable? (:st-dim/editable? dim)
             :db/id (temp-id-for-st-dim dim)
             :dimension/group dg-eid
             :source/imported-datetime (tu/now-jdate)}
            (merge (select-keys dim [:source/name :source/frgn-uuid :source/frgn-str-id])))
        (throw (Exception. (str "Не передано название для нового измерения. Описание измерения:"
                                dim))))
      (throw (Exception. (str "Не найдена группа с названием: " group-name))))))


(defn st-dim->tx-form
  "Получить транзакционную форму для стандартного измерения"
  [conn dim]
  (let [{dim-type        :st-dim/type
         src-frgn-uuid   :source/frgn-uuid
         src-frgn-str-id :source/frgn-str-id
         src-name        :source/name
         dim-name        :st-dim/name
         dg-name         :st-dim/group-name} dim]
    (-> (u/info-map m
          ;; entity уже существующего измерения
          :exist-e (or (m/e-of-foreign-dimension conn dim)   ; по foreign id
                       (when (:st-dim/name dim)              ; по названию
                         (du/attr-val->e-single conn
                                                :dimension/name
                                                (:st-dim/name dim))))
          ;; tx для уже существующего измерения
          :exist-tx-form (when (:exist-e m)
                          {:type :exists
                           :dim-e (:db/id (:exist-e m))})
          ;; конечная форма tx
          :result-tx-form
            (if (:exist-tx-form m)
              (:exist-tx-form m) ; если уже есть, просто возвращаем
              ;; иначе проверяем тип измерения
              (case dim-type
                ;; если обязательно должно уже существовать
                :must-pre-exist
                   ;; выкидываем исключение
                   (throw (Exception. (str "Не найдено обязательное измерение для uuid:"
                                           (:source/frgn-uuid dim) ", str-id: "
                                           (:source/frgn-str-id dim) ", name: "
                                           (:st-dim/name dim))))
                ;; если можно добавлять новое измерение
                :addable
                  {:type    :new
                   :pre-txs [(new-st-dim-txs conn dim)]
                   :dim-e   (temp-id-for-st-dim dim)}
                ;; если тип измерения не найден
                (throw (Exception. (str "Type " dim-type " of dim not found"))))))
        :result-tx-form)))


;; Получить транзакцию из стандартной записи
(defmethod ent->load-item [:st-entry :datomic-tx]
  [conn mapped-item mapped-item-type load-item-type]
  (let [e mapped-item]
    (println "ent->load-item")
    (println e)
    (if (= :D (:st-entry/op-type e))
      ;; при удалении
      (if-let [e-to-delete (m/e-of-foreign-entry conn e)]
        [[:db/retractEntity (:db/id e-to-delete)]]
        ;; TODO: стоит ли выкидывать исключение или просто игнорить?
        (do
          (println "IGNORE because couldn't find entry to delete: " e)
          :ignore))
        ; (throw (Exception. ()"Не найдена запись для удаления: " e)))
      ;; если новое или редактирование
      (-> (u/info-map m
            ;; транзакции по измерениям
            :dims-tx-forms (map #(st-dim->tx-form conn %) (:st-entry/dims e))
            ;; транзакция по самой записи
            :e-tx (-> {:entry/date      (:st-entry/date e)
                       :entry/summ      (:st-entry/summ e)
                       :entry/v-flow    (:st-entry/v-flow e)
                       :entry/v-type    (:st-entry/v-type e)
                       :entry/editable? (:st-entry/editable? e)
                       :entry/dims      (->> (:dims-tx-forms m)
                                             (map :dim-e)
                                             (into []))
                       :source/imported-datetime (tu/now-jdate)}
                      (merge (select-keys e [:source/name :source/frgn-uuid :source/frgn-str-id]))
                      (dissoc :op-type)
                      ;; если уже есть в базе - значит редактирование, иначе добавление
                      (#(if-let [exist-e (m/e-of-foreign-entry conn e)]
                           (assoc % :db/id (:db/id exist-e))
                           (assoc % :entry/uuid (d/squuid))))
                      (u/remove-nil-keys))
            :pre-txs (into [] (mapcat :pre-txs (:dims-tx-forms m)))
            ;; объединяем транзакции
            :result-txs (conj (:pre-txs m) (:e-tx m)))
          :result-txs))))

;; -- TESTING -----------------------------------------------------------------
; (def test-xml-item
;   {:UUID "568d03ea-be27-4e34-a86f-7ae703a66be4"
;    :Operation "D"
;    :Date nil
;    :Sum nil
;    :FlowType nil
;    :BankAccount nil
;    :Contract nil
;    :Contractor nil})
;   ; {:UUID "568d03ea-be27-4e34-a86f-7ae703a66be4"
;   ;  :Operation "U"
;   ;  :Date "2018-01-22"
;   ;  :Sum "13261737"
;   ;  :FlowType "inflow"
;   ;  :BankAccount {:Name "252160458( рос.рубли ЦКБ)"
;   ;                :UUID "ac1ca3e9-7014-11e2-8d6d-001d7da1f613"}
;   ;  :Contract   {:Name "Без договора"
;   ;               :UUID "bc1ca3e9-7014-11e2-8d6d-001d7da1f614"}})
;
;
; (def test-process-params
;  {:id   :processing-1c-entries
;   :name "Обработка xml от 1С"
;   :stages
;     [{:id :stage-etl-entries-1c
;       :name "ETL самих записей с xml"
;       :run-when :always
;       :action
;        {:action-type :etl-by-items
;         :load-params
;          {:type :datomic-tx}
;         :pnm-params
;          {:parse {:from :xml
;                   :item-params {:tag :Entry
;                                 :inner-fields  [:Operation
;                                                 :UUID
;                                                 :DocumentType
;                                                 :FlowType
;                                                 :Number
;                                                 :Date
;                                                 :Sum
;                                                 :Currency
;                                                 :SumInCurrency
;                                                 :CurrencyRate
;                                                 :Comment
;                                                 :Purpose
;                                                 :BookingAccount
;                                                 :IncomingNumber
;                                                 :IncomingDate
;                                                 [:Contractor [:Name :UUID]]
;                                                 [:Contract [:Name :UUID]]
;                                                 [:BankAccount [:Name :UUID]]]}}
;           :mapping
;            {:to :st-entry
;             :ignore-when :entry-date-before-2017?
;             :post-mapping :check-op-type-st-entry
;             :via {:method :mpvectors
;                   :params
;                    {:source/name        [:const "1С"]
;                     :source/frgn-uuid   [:field :UUID :uuid]
;                     :st-entry/op-type   [:field :Operation :match {:ms {"U" :U  "D" :D}}]
;                     :st-entry/date      [:field :Date :date]
;                     :st-entry/summ      [:field :Sum :double]
;                     :st-entry/v-flow    [:field :FlowType :match {:ms {"inflow" :inflow "outflow" :outflow}}]
;                     :st-entry/v-type    [:const :fact]
;                     :st-entry/editable? [:const false]
;                     :st-entry/dims      [:custom
;                                          :st-entry-dims
;                                          {:defaults {:source/name       [:const "1С"]
;                                                      :source/frgn-uuid  [:field :UUID :uuid]
;                                                      :st-dim/type       [:const :addable]
;                                                      :st-dim/name       [:field :Name :str]
;                                                      :st-dim/editable?  [:const false]}
;                                           :dims {:BankAccount {:st-dim/group-name [:const "Счета"]}
;                                                  :Contractor  {:st-dim/group-name [:const "Контрагенты"]}
;                                                  :Contract    {:st-dim/group-name [:const "Договоры"]}}}]}}}}}}]})
;
;
;
; (defn test-pnm
;   []
;   (let [pnm-params (get-in test-process-params [:stages 0 :action :pnm-params])]
;     (pnm/item->mapped-item test-xml-item pnm-params)))
;
;
; (defn test-l
;   []
;   (let [mapped-item (test-pnm)
;         conn (get-conn)]
;     (l/mapped-item->load-item conn mapped-item :st-entry :datomic-tx)))
;
;
; (defn test-vld
;   []
;   (let [mapped-item (test-pnm)
;         conn (get-conn)]
;     (validate-mapped-item conn mapped-item :st-entry)))
;
;
; (defn test-etl
;   []
;   (let [conn (get-conn)
;         item test-xml-item
;         pnm-params (get-in test-process-params [:stages 0 :action :pnm-params])
;         load-params (get-in test-process-params [:stages 0 :action :load-params])]
;     (etl/run-etl-item conn item pnm-params load-params)))
;
;
; (defn test-processing
;   []
;   (let [config test-process-params
;         conn (get-conn)]
;     (open-handlers/open-and-process
;       :xml :local-file
;       "import_files/pervichka_test.xml"
;       #(prcs/process-source-with-config conn
;                                         %
;                                         config)
;       {})))

; (s/explain ::recash-upload-console.data-upload.upload-specs/st-entry
;   {:source/frgn-uuid #uuid "568d03ea-be27-4e34-a86f-7ae703a66be4",
;    :source/name "1С",
;    :st-entry/op-type :D})

;; -- TEST JSON ---------------------------------------------------------------
; (def json-str
;   "{\"entries\": [{
;           \"opType\": \"U\",
;           \"id\":  \"selling-3242\",
;           \"date\": \"2017-07-11T10:37:00\",
;           \"flowType\": 0,
;           \"summ\": 3600.68,
;           \"client\": \"client43\",
;           \"store\": \"store34\",
;           \"account\": \"account5\"}]}")
;
; (def json-pnm-params
;    {:parse {:from :json
;             :item-params {:tags :entries}}
;     :mapping
;      {:to :st-entry
;       :post-mapping :check-op-type-st-entry
;       :via {:method :mpvectors
;             :params
;              {:source/name        [:const "ummastore"]
;               :source/frgn-str-id [:field :id :str]
;               :st-entry/op-type   [:field :opType :match {"U" :U  "D" :D}]
;               :st-entry/date      [:field :date :datetime]
;               :st-entry/summ      [:field :summ :double]
;               :st-entry/v-flow    [:field :flowType :match {1 :inflow 0 :outflow}]
;               :st-entry/v-type    [:const :fact]
;               :st-entry/editable? [:const false]
;               :st-entry/dims      [:custom
;                                    :st-entry-dims
;                                    {:defaults {:source/name        [:const "ummastore"]
;                                                :source/frgn-str-id [:field :_this :str]
;                                                :st-dim/type        [:const :addable]
;                                                :st-dim/editable?   [:const false]}
;                                     :dims {:client  {:st-dim/group-name [:const "Договоры"]}
;                                            :account {:st-dim/group-name [:const "Контрагенты"]}
;                                            :store   {:st-dim/group-name [:const "Статьи"]}}}]}}}})
;
; (def test-json-item
;   {:flowType 0
;    :date "2017-07-11T10:37:00",
;    :summ 3600.68,
;    :store "store34",
;    :client "client43",
;    :account "account5",
;    :opType "U",
;    :id "selling-3242"})
;
;
; (defn test-json-mapping
;   []
;   (pnm/item->mapped-item test-json-item json-pnm-params))
;
;
; (defn test-json-l
;   []
;   (let [mapped-item (test-json-mapping)
;         conn (get-conn)]
;     (l/mapped-item->load-item conn mapped-item :st-entry :datomic-tx)))
















; (defn t-loading
;   []
;   (mapped-item->load-item (get-conn)
;                           (pnm/tt)
;                           :st-entry
;                           :datomic-tx))

;
; (def test-pnm-params
;   {:parse {:from :xml
;            :item-params {:tag :Entry
;                          :inner-fields [:Operation :UUID :DocumentType]}}
;    :mapping
;     {:to :st-entry
;      :ignore-when :entry-date-before-2017?
;      :via {:method :mpvectors
;            :params
;             {:source/name        [:const "1С"]
;              :source/frgn-uuid   [:field :UUID :uuid]
;              :st-entry/op-type   [:field :Operation :match {"U" :U  "D" :D}]
;              :st-entry/date      [:field :Date :date]
;              :st-entry/summ      [:field :Sum :double]
;              :st-entry/v-flow    [:field :FlowType :match {"inflow" :inflow "outflow" :outflow}]
;              :st-entry/v-type    [:const :fact]
;              :st-entry/editable? [:const false]
;              :st-entry/dims      [:custom
;                                   :st-entry-dims
;                                   {:defaults {:source/name       [:const "1С"]
;                                               :source/frgn-uuid  [:field :UUID :uuid]
;                                               :st-dim/type       [:const :addable]
;                                               :st-dim/name       [:field :Name :str]
;                                               :st-dim/editable?  [:const false]}
;                                    :dims {:BankAccount {:st-dim/group-name [:const "Счета"]}}}]}}}})
;                                           ; :Contractor  {:st-dim/group-name [:const "Контрагенты"]}
;                                           ; :Contract    {:st-dim/group-name [:const "Договоры"]}}}]}}}})
;
;
;
; (def test-xml-item
;   {:UUID "568d03ea-be27-4e34-a86f-7ae703a66be4"
;    :Operation "U"
;    :Date "2018-01-22"
;    :Sum "13261737"
;    :FlowType "inflow"
;    :BankAccount {:Name "252160458( рос.рубли ЦКБ)"
;                  :UUID "ac1ca3e9-7014-11e2-8d6d-001d7da1f613"}})
;
;
; (defn tt
;   []
;   (item->mapped-item test-xml-item
;                      test-pnm-params))
;
;
; (s/valid? :recash-upload-console.processings.upload-specs/st-entry
;           (tt))












;
; (def db-uri
;   "datomic:sql://recash-local?jdbc:postgresql://localhost:5432/recash-local?user=datomic&password=datomic")
;
; (defn get-conn
;   []
;   (dbm/new-conn db-uri))
;
;
;
; (def st-entry-xml-mappings
;   {:source/name        [:const "1С"]
;    :source/frgn-uuid   [:field :UUID :uuid]
;    :st-entry/op-type   [:field :Operation :match {"U" :U  "D" :D}]
;    :st-entry/date      [:field :Date :date]
;    :st-entry/summ      [:field :Sum :double]
;    :st-entry/v-flow    [:field :FlowType :match {"inflow" :inflow "outflow" :outflow}]
;    :st-entry/v-type    [:const :fact]
;    :st-entry/editable? [:const false]
;    :st-entry/dims      [:custom
;                         :st-entry-dims
;                         {:defaults {:source/name       [:const "1С"]
;                                     :source/frgn-uuid  [:field :UUID :uuid]
;                                     :st-dim/type       [:const :addable]
;                                     :st-dim/name       [:field :Name :str]
;                                     :st-dim/editable?  [:const false]}
;                          :dims {:BankAccount {:st-dim/group-name [:const "Счета"]}}}]})
;                                 ; :Contractor  {:st-dim/group-name [:const "Контрагенты"]}
;                                 ; :Contract    {:st-dim/group-name [:const "Договоры"]}}}]})
;
;
; (defmulti parsing-custom
;   (fn [item mapping-for parse-custom-key parse-params] parse-custom-key))
;
; (defmulti item-has-field?
;   (fn [mapping-for item field] mapping-for))
;
;
; (defmethod item-has-field? :xml
;   [mapping-for item field]
;   (contains? item field))
;
; (defmethod item-has-field? :csv
;   [mapping-for item field]
;   (<= field (count item)))
;
; (defmulti field-from-item
;   (fn [mapping-for item field] mapping-for))
;
;
; (defmethod field-from-item :xml
;   [mapping-for item field]
;   (get item field))
;
; (defmethod field-from-item :csv
;   [mapping-for item field]
;   (nth item field))
;
;
; (defn item->via-mv->field
;   [item mapping-v mapping-for]
;   (let [parse-type (first mapping-v)]
;     (case parse-type
;       ;; берем константное значение
;       :const (let [[_ val] mapping-v]
;                val)
;       ;; считываем поле из источника
;       :field (let [[_ field parse-format parse-params] mapping-v]
;                (->> item
;                     ;; если :_this
;                     (#(if (= :_this field)
;                         %                ; то берем само значение,
;                         (if (item-has-field? mapping-for % field) ; TODO: единую форму
;                           (field-from-item mapping-for % field)  ; берем внутреннее поле
;                           (throw (Exception. (str "Not found field: " field " in item: " item))))))
;                     ;; парсим по полю
;                     (upl/parse-by-type {:type parse-format
;                                         :params (if (= parse-format :match)
;                                                   ;; TODO: сделать единую форму
;                                                   {:matches parse-params}
;                                                   parse-params)})))
;       :custom (let [[_ parse-custom-key parse-params] mapping-v]
;                 (parsing-custom item mapping-for parse-custom-key parse-params)))))
;
;
; (def test-item
;   {:UUID "568d03ea-be27-4e34-a86f-7ae703a66be4"
;    :Operation "U"
;    :Date "2013-01-22"
;    :Sum "13261737"
;    :FlowType "inflow"
;    :BankAccount {:Name "252160458( рос.рубли ЦКБ)"
;                  :UUID "ac1ca3e9-7014-11e2-8d6d-001d7da1f613"}})
;
;
;
; (defn item->via-mv->mapped-item
;   [item mappings-m mapping-for]
;   (reduce-kv (fn [nm mapping-k mapping-v]
;                (assoc nm mapping-k (item->via-mv->field item mapping-v mapping-for)))
;              {} mappings-m))
;
;
;
; (defmethod parsing-custom :st-entry-dims
;   [item mapping-for parse-custom-key parse-params]
;   (reduce-kv (fn [result-dims dk d-mapping]
;                (if-not (item-has-field? mapping-for item dk)
;                  (throw (Exception. (str "Not found field: " dk " in item: " item)))
;                  (let [current-dim-item (field-from-item mapping-for item dk)
;                        new-st-dim (when-not (u/nil-or-empty-or-blank? current-dim-item)
;                                     (-> current-dim-item
;                                         (item->via-mv->mapped-item (:defaults parse-params) mapping-for)
;                                         (merge (item->via-mv->mapped-item current-dim-item d-mapping mapping-for))))]
;                    (if new-st-dim
;                      (conj result-dims new-st-dim)
;                      result-dims))))
;              [] (:dims parse-params)))
;
;
; (def mapped-item
;   (item->via-mv->mapped-item
;     test-item
;     st-entry-xml-mappings
;     :xml))
;
; ; (s/explain ::recash-upload-console.data-upload.upload-specs/st-entry
; ;            mapped-item)
;
;
; ; (defn item->st-dims-of-st-entry
; ;   [item dims-mapping]
; ;   (-> (reduce-kv (fn [result-dims dk d-mapping]
; ;                    (let [new-st-dim (->)])))))
;
;
;
;
; (def st-entry-csv-mappings
;   {:source/name        [:const "local-csv"]
;    :st-entry/op-type   [:const :U]
;    :st-entry/v-flow    [:field 0 :match {"Приток" :inflow
;                                          "Отток" :outflow}]
;    :st-entry/summ      [:field 5 :double]
;    :st-entry/date      [:field 6 :date]
;    :st-entry/v-type    [:const :fact]
;    :st-entry/editable? [:const false]
;    :st-entry/dims      [:custom
;                         :st-entry-dims
;                         {:defaults {:source/name       [:const "local-xml"]
;                                     :st-dim/type       [:const :must-pre-exist]
;                                     :st-dim/name       [:field :_this :str]
;                                     :st-dim/editable?  [:const false]}
;                          :dims
;                           {1 {:st-dim/group-name [:const "Счета"]}
;                            3 {:st-dim/group-name [:const "Контрагенты"]}
;                            4 {:st-dim/group-name [:const "Договоры"]}}}]})
;
; (def test-csv-entry
;   ["Приток" "serq" "ignorestatiya" "" "Без договора" "34234.4" "2017-07-07"])
;
; (def mapped-csv-item
;   (item->via-mv->mapped-item
;     test-csv-entry
;     st-entry-csv-mappings
;     :csv))
;
;
; (def st-entry-json-mappings
;   {:source/name        [:const "ummastore-web-app"]
;    :source/frgn-str-id [:field :id :string]
;    :st-entry/op-type   [:field :opType :match {"U" :U  "D" :D}]
;    :st-entry/date      [:field :date :date]
;    :st-entry/summ      [:field :summ :double]
;    :st-entry/v-flow    [:field :flowType :match {1 :inflow
;                                                  0 :outflow}]
;    :st-entry/v-type    [:const :fact]
;    :st-entry/editable? [:const false]
;    :st-entry/dims      {:defaults {:source/name        [:const "ummastore-web-app"]
;                                    :source/frgn-str-id [:field :_this :string]
;                                    :st-dim/type        [:const :must-pre-exist]
;                                    :st-dim/editable?   [:const false]}
;                         :dims
;                          {:client   {:st-dim/group-name [:const "Клиенты"]}
;                           :store    {:st-dim/group-name [:const "Магазины"]}
;                           :account  {:st-dim/group-name [:const "Кассы UmmaStore"]}
;                           :category {:st-dim/group-name [:const "Категория UmmaStore"]}}}})
;
;
;
; (def convert-to-st-entry
;   {:source/name        [:const "1С"]
;    :source/frgn-uuid   [:field :uuid-1c]
;    :st-entry/op-type   [:field :op-type]
;    :st-entry/date      [:field :date]
;    :st-entry/summ      [:field :summ]
;    :st-entry/v-flow    [:field :v-flow]
;    :st-entry/v-type    [:const :fact]
;    :st-entry/editable? [:const false]
;    :st-entry/dims      {:source/name       [:const "1С"]
;                         :source/frgn-uuid  [:field :uuid-1c]
;                         :st-dim/type       [:const :addable]
;                         :st-dim/name       [:field :name]
;                         :st-dim/group-name [:field :group-name]
;                         :st-dim/editable?  [:const false]}})
;
; (defn id-pair-for-foreign-entity
;   [e]
;   (if-let [uuid (:source/frgn-uuid e)] ; приоритет у uuid
;     [:source/frgn-uuid uuid]
;     [:source/frgn-str-id  (:source/frgn-uuid e)]))
;
;
; (defn e-of-foreign-entity
;   "Получить entity импортированной сущности"
;   [e-uuid conn e]
;   (let [[id-attr id-val] (id-pair-for-foreign-entity e)
;         src-name (:source/name e)
;         db (d/db conn)
;         query {:find '[?e]
;                :in '[$ ?id ?src-name]
;                :where [['?e e-uuid]
;                        ['?e id-attr '?id]
;                        ['?e :source/name '?src-name]]}]
;     (when id-val
;       (->> (d/q query db id-val src-name)
;            first
;            (d/entity db)
;            not-empty))))
;
;
; (def e-of-foreign-entry (partial e-of-foreign-entity :entry/uuid))
; (def e-of-foreign-dimension (partial e-of-foreign-entity :dimension/uuid))
;
;
; (defn temp-id-for-st-dim
;   "Получить временный id для измерения"
;   [dim]
;   (str (:source/name dim)
;        "_"
;        (or (:source/frgn-uuid dim)
;            (:source/frgn-str-id dim)
;            (:st-dim/name dim))))
;
;
; (defn new-st-dim-txs
;   "Получить датомы для добавления нового стандартного измерения"
;   [conn dim]
;   (let [group-name (:st-dim/group-name dim)]
;     (if-let [dg-eid (:db/id (du/attr-val->e-single conn :dim-group/name group-name))]
;       (-> {:dimension/uuid (d/squuid)
;            :dimension/name (:st-dim/name dim)
;            :dimension/editable? (:st-dim/editable? dim)
;            :db/id (temp-id-for-st-dim dim)
;            :dimension/group dg-eid
;            :source/imported-datetime (tu/now-jdate)}
;           (merge (select-keys dim [:source/name :source/frgn-uuid :source/frgn-str-id])))
;       (throw (Exception. (str "Не найдена группа с названием: " group-name))))))
;
;
; (defn standard-dim->tx-form
;   "Получить транзакционную форму для стандартного измерения"
;   [conn dim]
;   (let [{dim-type        :st-dim/type
;          src-frgn-uuid   :source/frgn-uuid
;          src-frgn-str-id :source/frgn-str-id
;          src-name        :source/name
;          dim-name        :st-dim/name
;          dg-name         :st-dim/group-name} dim]
;     (-> (u/info-map m
;           ;; entity уже существующего измерения
;           :exist-e (or (e-of-foreign-dimension conn dim)     ; по foreign id
;                        (du/attr-val->e-single conn           ; по названию
;                                               :dimension/name
;                                               (:st-dim/name dim)))
;           ;; tx для уже существующего измерения
;           :exist-tx-form (when (:exist-e m)
;                           {:type :exists
;                            :dim-e (:db/id (:exist-e m))})
;           ;; конечная форма tx
;           :result-tx-form
;             (if (:exist-tx-form m)
;               (:exist-tx-form m) ; если уже есть, просто возвращаем
;               ;; иначе проверяем тип измерения
;               (case dim-type
;                 ;; если обязательно должно уже существовать
;                 :must-pre-exist
;                    ;; выкидываем исключение
;                    (throw (Exception. (str "Не найдено обязательное измерение для uuid:"
;                                            (:source/frgn-uuid dim) ", str-id: "
;                                            (:source/frgn-str-id dim) ", name: "
;                                            (:st-dim/name dim))))
;                 ;; если можно добавлять новое измерение
;                 :addable
;                   {:type    :new
;                    :pre-txs [(new-st-dim-txs conn dim)]
;                    :dim-e   (temp-id-for-st-dim dim)}
;                 ;; если тип измерения не найден
;                 (throw (Exception. (str "Type " dim-type " of dim not found"))))))
;         :result-tx-form)))
;
;
; (defn standard-entry->tx-form
;   "Получить транзакцию из стандартной записи"
;   [conn e]
;   (if (= :ignore e)
;     :ignore ; если нужно проигнорировать
;     ;; при обычных обстоятельствах
;     (if (= :D (:st-entry/op-type e))
;       ;; при удалении
;       [[:db/retractEntity (e-of-foreign-entity conn e)]]
;       ;; если новое или редактирование
;       (-> (u/info-map m
;             ;; транзакции по измерениям
;             :dims-tx-forms (map #(standard-dim->tx-form conn %) (:st-entry/dims e))
;             ;; транзакция по самой записи
;             :e-tx (-> {:entry/date      (:st-entry/date e)
;                        :entry/summ      (:st-entry/summ e)
;                        :entry/v-flow    (:st-entry/v-flow e)
;                        :entry/v-type    (:st-entry/v-type e)
;                        :entry/editable? (:st-entry/editable? e)
;                        :entry/dims      (->> (:dims-tx-forms m)
;                                              (map :dim-e)
;                                              (into []))
;                        :source/imported-datetime (tu/now-jdate)}
;                       (merge (select-keys e [:source/name :source/frgn-uuid :source/frgn-str-id]))
;                       (dissoc :op-type)
;                       ;; если уже есть в базе - значит редактирование, иначе добавление
;                       (#(if-let [exist-e (e-of-foreign-entry conn e)]
;                            (assoc % :db/id exist-e)
;                            (assoc % :entry/uuid (d/squuid))))
;
;                       (u/remove-nil-keys))
;             :pre-txs (into [] (mapcat :pre-txs (:dims-tx-forms m)))
;             ;; объединяем транзакции
;             :result-txs (conj (:pre-txs m) (:e-tx m)))
;           :result-txs))))
;
;
;
; (s/explain :recash-upload-console.data-upload.upload-specs/st-entry
;            mapped-csv-item)
;
; (defn test-csv
;   []
;   (standard-entry->tx-form
;     (get-conn)
;     mapped-csv-item))
;
;
; (def test-1c-pp
;   {:item-tag   :Entry
;    :inner-fields   [:Operation
;                     :UUID
;                     :DocumentType
;                     :FlowType
;                     :Number
;                     :Date
;                     :Sum
;                     :Currency
;                     :SumInCurrency
;                     :CurrencyRate
;                     :Comment
;                     :Purpose
;                     :BookingAccount
;                     :IncomingNumber
;                     :IncomingDate
;                     [:Contractor [:Name :UUID]]
;                     [:Contract [:Name :UUID]]
;                     [:BankAccount [:Name :UUID]]]})
;
;
; (def new-1c-test-item
;   (let [e-tag        (:item-tag test-1c-pp)
;         inner-fields (:inner-fields test-1c-pp)
;         str-source
;           "<Entry>
;             <UUID>2d32b7dd-f69d-11e3-8281-00270e03a4fc</UUID>
;             <Operation>U</Operation>
;             <DocumentType>Платежное поручение исходящее</DocumentType>
;             <FlowType>outflow</FlowType>
;             <Number>000373     </Number>
;             <Date>2014-06-20</Date>
;             <Sum>34693.1</Sum>
;             <Currency>KZT</Currency>
;             <CurrencyRate>1</CurrencyRate>
;             <SumInCurrency>34693.1</SumInCurrency>
;             <BookingAccount>1030</BookingAccount>
;             <IncomingNumber/>
;             <IncomingDate>0001-01-01</IncomingDate>
;             <Organization>
;               <UUID>b8da4b27-d788-4117-9df1-38f47c9d4aa4</UUID>
;               <Name>Национальный филиал МТРК \"Мир\" в РК</Name>
;             </Organization>
;             <BankAccount>
;               <UUID>8eef5d30-7c0b-11e3-a339-902b34bf17fe</UUID>
;               <Name>KZ71070KK1KS03771004 в ГУ Комитет казначейства Мин</Name>
;             </BankAccount>
;             <Contractor>
;               <UUID>9047ccf2-00bf-11e3-86dc-00270e03a4fc</UUID>
;               <Name>Рубежанская Ольга Владимировна</Name>
;             </Contractor>
;             <Category>
;               <UUID>078aad36-7016-11e2-8d6d-001d7da1f613</UUID>
;               <Name>Алименты</Name>
;             </Category>
;             <Purpose>
;         Сумма 34 693-10 тенге в т.ч. НДС(12%) 0-00 тенге Алименты за май 2014</Purpose>
;             <Comment/>
;           </Entry>"
;         ;; предполагается, что xml/parse и zip/xml-zip делались выше
;         zipped-source (xml-u/str->zipped-source str-source)]
;     (-> zipped-source
;         (xml-u/zipper->inners e-tag)
;         (#(map (fn [z] (upl/zipper->item z inner-fields))
;                %))
;         first)))
;
;
; (standard-entry->tx-form
;   (get-conn)
;   (item->via-mv->mapped-item
;     new-1c-test-item
;     st-entry-xml-mappings
;     :xml))
;
; (defmulti ignore-parsing?
;   (fn [item pred-key] pred-key))
;
;
; (defmethod ignore-parsing? :entry-date-before-2017?
;   [item pred-key]
;   (println "ignore-parsing? :entry-date-before-2017?")
;   (println "item: " item)
;   (let [date (upl/parse-by-type {:type :date} (:Date item))]
;     (tu/jdates-before? date (tu/jdate 2017 1 1))))
;
;
; (defn parse-item
;   [parse-params item]
;   (let [{:keys [parse-type parse-to ignore-parsing-when mappings]} parse-params]
;     (-> (u/info-map m
;           :item item
;           :ignore-parse? (if ignore-parsing-when
;                            (ignore-parsing? item ignore-parsing-when)
;                            false)
;           :mapped-item (if-not (:ignore-parse? m)
;                          (item->via-mv->mapped-item item mappings parse-type)
;                          :ignore))
;         :mapped-item)))
;
;
; ; (defmethod validate-ent :st-entry
; ;   [conn ek e]
; ;   (if (s/valid? ::recash-upload-console.data-upload.upload-specs/st-entry e)
; ;     []
; ;     [(s/explain-str ::recash-upload-console.data-upload.upload-specs/entry-1c-spec e)]))
;
;
; (def mappings-params
;   {:parse-type :xml
;    :item-params {:tag :Entry
;                  :inner-fields [:Operation :UUID :DocumentType]}
;    :ignore-mapping-when :entry-date-before-2017?
;    :mapping-to :st-entry
;    :mapping-via :via-mappingvectors
;    :mappings
;     {:source/name        [:const "1С"]
;      :source/frgn-uuid   [:field :UUID :uuid]
;      :st-entry/op-type   [:field :Operation :match {"U" :U  "D" :D}]
;      :st-entry/date      [:field :Date :date]
;      :st-entry/summ      [:field :Sum :double]
;      :st-entry/v-flow    [:field :FlowType :match {"inflow" :inflow "outflow" :outflow}]
;      :st-entry/v-type    [:const :fact]
;      :st-entry/editable? [:const false]
;      :st-entry/dims      [:custom
;                           :st-entry-dims
;                           {:defaults {:source/name       [:const "1С"]
;                                       :source/frgn-uuid  [:field :UUID :uuid]
;                                       :st-dim/type       [:const :addable]
;                                       :st-dim/name       [:field :Name :str]
;                                       :st-dim/editable?  [:const false]}
;                            :dims {:BankAccount {:st-dim/group-name [:const "Счета"]}
;                                   :Contractor  {:st-dim/group-name [:const "Контрагенты"]}
;                                   :Contract    {:st-dim/group-name [:const "Договоры"]}}}]}})
;
;
;
; (def local-xml-1c-parsing
;  {:parse-type :xml
;   :item-tag   :Entry
;   :inner-fields  [:Operation
;                   :UUID
;                   :DocumentType
;                   :FlowType
;                   :Number
;                   :Date
;                   :Sum
;                   :Currency
;                   :SumInCurrency
;                   :CurrencyRate
;                   :Comment
;                   :Purpose
;                   :BookingAccount
;                   :IncomingNumber
;                   :IncomingDate
;                   [:Contractor [:Name :UUID]]
;                   [:Contract [:Name :UUID]]
;                   [:BankAccount [:Name :UUID]]]
;   :parse-to :st-entry
;   :ignore-parsing-when  :entry-date-before-2017?
;   :mappings
;     {:source/name        [:const "1С"]
;      :source/frgn-uuid   [:field :UUID :uuid]
;      :st-entry/op-type   [:field :Operation :match {"U" :U  "D" :D}]
;      :st-entry/date      [:field :Date :date]
;      :st-entry/summ      [:field :Sum :double]
;      :st-entry/v-flow    [:field :FlowType :match {"inflow" :inflow "outflow" :outflow}]
;      :st-entry/v-type    [:const :fact]
;      :st-entry/editable? [:const false]
;      :st-entry/dims      [:custom
;                           :st-entry-dims
;                           {:defaults {:source/name       [:const "1С"]
;                                       :source/frgn-uuid  [:field :UUID :uuid]
;                                       :st-dim/type       [:const :addable]
;                                       :st-dim/name       [:field :Name :str]
;                                       :st-dim/editable?  [:const false]}
;                            :dims {:BankAccount {:st-dim/group-name [:const "Счета"]}
;                                   :Contractor  {:st-dim/group-name [:const "Контрагенты"]}
;                                   :Contract    {:st-dim/group-name [:const "Договоры"]}}}]}})
;
;
;
; (parse-item local-xml-1c-parsing
;             new-1c-test-item)

; (upl/get-items-from-source stage-config source)
;
;
; (open-handlers/open-and-process
;   :xml :open-type
;   (second proc-type)
;   (:file-name proc)
;   (fn [d]
;     (u-s/process-source-with-config conn
;                                     upl-config
;                                     d))
;   {})

; (standard-entry->tx-form
;   (get-conn)
;   mapped-item)

; (standard-dim->tx-form
;   (get-conn)
;   {:st-dim/type        :addable
;    :st-dim/name        "Без договора565"
;    :st-dim/group-name  "Договоры"
;    :st-dim/editable?   false
;    :source/name        "1C"
;    :source/frgn-uuid   #uuid "568d03ea-be27-4e34-a86f-7ae703a66be4"})

; (standard-entry->tx-form
;   (get-conn)
;   {:st-entry/uuid      #uuid "498d03ea-be27-4e34-a86f-7ae703a66bef"
;    :st-entry/op-type   :U ; #{:U :D}
;    :st-entry/date      #inst "2017-04-06T00:00:00"
;    :st-entry/summ      45453.65
;    :st-entry/v-flow    :inflow
;    :st-entry/v-type    :fact
;    :st-entry/editable? false
;    :source/name        "ummastore"
;    :source/frgn-str-id "sales-55"
;    :source/frgn-uuid   #uuid "1c8d03ea-be27-4e34-a86f-7ae703a66bef"
;    :st-entry/dims      [{:st-dim/type        :addable
;                          :st-dim/name        "name cntr"
;                          :st-dim/group-name  "Контрагенты"
;                          :st-dim/editable?   false
;                          :source/name        "1C"
;                          :source/frgn-uuid   #uuid "568d03ea-be27-4e34-a86f-7ae703a66be5"}
;                         {:st-dim/type        :addable
;                          :st-dim/name        "Без договора"
;                          :st-dim/group-name  "Договоры"
;                          :st-dim/editable?   false
;                          :source/name        "1C"
;                          :source/frgn-uuid   #uuid "568d03ea-be27-4e34-a86f-7ae703a66be4"}]})


; {:st-dim/uuid        #uuid "498d03ea-be27-4e34-a86f-7ae703a66be4"
;  :st-dim/type        :must-pre-exist ; #{:must-pre-exist :addable}
;  :st-dim/name        "Договор-234234"
;  :st-dim/group-name  "Договоры"
;  :st-dim/editable?   false
;  :source/name        "1C"
;  :source/frgn-uuid   #uuid "568d03ea-be27-4e34-a86f-7ae703a66be4"
;  :source/frgn-str-id "dogovor-44234"}
;
; {:st-entry/uuid      #uuid "498d03ea-be27-4e34-a86f-7ae703a66bef"
;  :st-entry/op-type   :U ; #{:U :D}
;  :st-entry/date      #inst "2017-04-06T00:00:00"
;  :st-entry/summ      45453.65
;  :st-entry/v-flow    :inflow ; #{:inflow :outflow}
;  :st-entry/v-type    :fact ; #{:plan :fact}
;  :st-entry/editable? false
;  :source/name        "ummastore"
;  :source/frgn-str-id "sales-55"
;  :source/frgn-uuid   #uuid "1c8d03ea-be27-4e34-a86f-7ae703a66bef"
;  :st-entry/dims      []} ; см. st-dim

; (comment
;   {:st-entry/date #inst "2017-04-06T00:00:00"
;    :st-entry/frgn-str-id "ummastore-sales-34"
;    :st-entry/frgn-uuid #uuid "3435-sdfsd-5345-dgdf"
;    :st-entry/frgn-source "ummastore"
;    :st-entry/op-type :U ; #{:U :D}
;    :st-entry/summ 435435.65
;    :st-entry/v-flow :inflow
;    :st-entry/v-type :fact
;    :st-entry/editable? false
;    :st-entry/dims [{:st-dim/type :pre-exist
;                     :st-dim/name "Новый контрагент"
;                     :st-dim/ext-str-id "client-34234"
;                     :st-dim/ext-uuid #uuid "324a-54546-7567-876c"
;                     :st-dim/group-name "Контрагенты"}
;                    {:st-dim/name "Старый договор с новым названием"
;                     :st-dim/ext-str-id "dogovor-44234"
;                     :st-dim/ext-uuid #uuid "3245-ca546-7567-876c"
;                     :st-dim/group-name "Договоры"}]}
;
;   ;; если только по существующим id
;   {:st-dim/type :pre-exist
;    :st-dim/frgn-str-id "client-342342"
;    :st-dim/frgn-source
;    :st-dim/group-name "Клиенты"}
;
;   ;; если по uuid и можно добавлять новые
;   {:st-dim/type :addable
;    :st-dim/frgn-uuid #uuid "45345-4534-g56df-sdsf"
;    :st-dim/name "Договор №5454-2017"
;    :st-dim/group-name "Договоры"}
;
;   ;; если только по названиям и можно добавлять новые
;   {:st-dim/type :addable
;    :st-dim/name "Новый контрагент"
;    :st-dim/group-name "Контрагент"})



; (defn standard-entry->tx-form
;   [conn e]
;   (if (= :D (:st-entry/op-type e))
;     ;; при удалении
;     [[:db/retractEntity (id-of-st-entry e)]]
;     ;; если новое или редактирование
;     (let [dims-txs (map #(standard-dim->tx-form conn %) (:dims e))
;           e-tx (-> e
;                    (assoc :dims (->> dims-txs
;                                      (map :tx)
;                                      (into [])))
;                    (dissoc :op-type)
;                    ;; добавляем префикс entry
;                    (u/update-keys #(->> % (name) (str "entry/") keyword))
;                    (assoc :entry/upload-date-1c (tu/now-jdate))
;                    ;; если уже есть с таким uuid-1c - значит редактирование, иначе добавление
;                    (#(if-let [exist-uuid (du/field-by-other-field :entry/uuid
;                                                                   :entry/uuid-1c
;                                                                   (:entry/uuid-1c %)
;                                                                   conn)]
;                         (assoc % :entry/uuid exist-uuid)
;                         (assoc % :entry/uuid (d/squuid))))
;                    (u/remove-nil-keys))
;           ;; транзакции для предв. создания новых измерений
;           pre-txs (into [] (mapcat :pre-txs dims-txs))]
;         ;; объединяем транзакции
;         (conj pre-txs e-tx))))
