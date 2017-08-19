(ns recash-upload-console.data-upload.upload-1c
  "Для загрузки данных с 1С"
  (:require [recash-upload-console.data-upload.upload-service
              :refer [parse-custom
                      validate-ent
                      transform-post
                      process-self
                      ent->tx-form
                      process-source
                      get-field-from-item
                      get-items-from-source] :as u-s]
            [recash-upload-console.common.parsing-utils :as pu]
            [recash-upload-console.common.utils :as u]
            [recash-upload-console.common.time-utils :as tu]
            [recash-upload-console.domain.model :as m]
            [recash-upload-console.domain.datomic-utils :as du]
            [datomic.api :as d]
            [clojure.string :as cljstr]
            [recash-upload-console.common.xml-utils :as xml-u]
            [clojure.data.xml :as xml]
            [clojure.zip :as zip]
            [clojure.java.io :as io]
            [clojure.data.zip.xml :as zip-xml]
            [clojure.spec :as s]
            [io.pedestal.log :as log]
            [recash-upload-console.data-upload.upload-specs]
            [recash-upload-console.common.open-handlers :as open-handlers]))


;; -- Парсинг полей -----------------------------------------------------------
(defmethod parse-custom [:entry-1c :dims]
  [ek mapping-params v]
  {:uuid-1c (java.util.UUID/fromString (:UUID v))
   :name (-> v :Name cljstr/trim)
   :group-name (get-in mapping-params [:params :group-name])})


(defmethod transform-post :transform-1c-entry
  [ek transform-params parsed-item]
  (if (= :D (:op-type parsed-item))
    (select-keys parsed-item [:uuid-1c :op-type])
    (-> parsed-item
        (merge transform-params))))


(defmethod validate-ent :entry-1c
  [conn ek e]
  (if (s/valid? ::recash-upload-console.data-upload.upload-specs/entry-1c-spec e)
    []
    [(s/explain-str ::recash-upload-console.data-upload.upload-specs/entry-1c-spec e)]))


(defn temp-1c-dim-id
  "Получить временный id для нового измерения"
  [uuid-1c]
  (str "1c-" uuid-1c))


(defn new-1c-dim-tx
  "Транзакция для добавления нового измерения"
  [conn dim-name group-name uuid-1c]
  (if-let [dg-id (m/dim-group-name->uuid group-name conn)]
    (-> {:dimension/uuid (d/squuid)
         :dimension/name dim-name
         :dimension/uuid-1c uuid-1c
         :db/id (temp-1c-dim-id uuid-1c)
         :dimension/group [:dim-group/uuid dg-id]})
    (throw (Exception. (str "Не найдена группа с названием: " group-name)))))


(defn dim-1c->tx-form
  [conn dim]
  (let [{dim-name :name group-name :group-name uuid-1c :uuid-1c} dim]
    ;; есть ли уже такое измерение в базе (по uuid-1c)
    (if-let [uuid (m/dimension-uuid-1c->uuid uuid-1c conn)]
      ;; уже есть
      (-> {:type :exists
           :tx   [:dimension/uuid-1c uuid-1c]}
          ;; проверяем название
          (#(if (not= (du/field-by-other-field :dimension/name
                                               :dimension/uuid-1c
                                               uuid-1c
                                               conn)
                      dim-name)
              ;; если несовпадающее название
              (assoc % :pre-txs [{:dimension/name dim-name
                                  :db/id [:dimension/uuid-1c uuid-1c]}])
              ;; если названия совпали
              %)))
      ;; еще нет, нужно добавить
      {:type    :new
       :pre-txs [(new-1c-dim-tx conn dim-name group-name uuid-1c)]
       :tx      (temp-1c-dim-id uuid-1c)})))


(defmethod ent->tx-form :entry-1c
  [conn ek e]
  (if (= :D (:op-type e))
    ;; при удалении
    [[:db/retractEntity [:entry/uuid-1c (:uuid-1c e)]]]
    ;; если валидная дата, чтобы игнорировать записи до 2017
    ;; TODO: продумать общий механизм?
    (if (tu/jdates-before? (:date e) (tu/jdate 2017 1 1))
      :ignore
      ;; если новое или редактирование
      (let [dims-txs (map #(dim-1c->tx-form conn %) (:dims e))
            e-tx (-> e
                     (assoc :dims (->> dims-txs
                                       (map :tx)
                                       (into [])))
                     (dissoc :op-type)
                     ;; добавляем префикс entry
                     (u/update-keys #(->> % (name) (str "entry/") keyword))
                     (assoc :entry/upload-date-1c (tu/now-jdate))
                     ;; если уже есть с таким uuid-1c - значит редактирование, иначе добавление
                     (#(if-let [exist-uuid (du/field-by-other-field :entry/uuid
                                                                    :entry/uuid-1c
                                                                    (:entry/uuid-1c %)
                                                                    conn)]
                          (assoc % :entry/uuid exist-uuid)
                          (assoc % :entry/uuid (d/squuid))))
                     (u/remove-nil-keys))
            ;; транзакции для предв. создания новых измерений
            pre-txs (into [] (mapcat :pre-txs dims-txs))]
          ;; объединяем транзакции
          (conj pre-txs e-tx)))))


(defn package-already-processed?
  "Проверка, что пакет был обработан"
  [conn package-number]
  (not (empty? (d/q '[:find ?e
                      :in $ ?pkg-number
                      :where [?e :package-1c/uuid]
                             [?e :package-1c/number ?pkg-number]
                             [?e :package-1c/status (db/ident :success)]]
                     (d/db conn) package-number))))


(defn- write-package-to-history
  "Записать обработку пакета в историю"
  [conn package-number status]
  (du/transact-and-return conn [{:package-1c/uuid (d/squuid)
                                 :package-1c/status status
                                 :package-1c/number package-number
                                 :package-1c/processing-date (tu/now-jdate)}]))

;; -- Обработка запросов с 1С -------------------------------------------------

(defn- failure-result
  "Вернуть отрицательный ответ (xml-строку)"
  [error]
  [:result {}
    [:status {} "failure"]
    [:error {} error]])


(defn- success-result
  "Вернуть положительный ответ (xml-строку)"
  []
  [:result {}
    [:status {} "success"]])


(defn check-package-status
  "Проверка пакета от 1С: был ли он уже обработан"
  [conn package-id]
  (try
    ;; если передан нормально id
    (if package-id
      ;; если такого нет - то положительный ответ, иначе отрицательный
      (if (not (package-already-processed? conn package-id))
        (success-result)
        (failure-result "package already processed"))
      (failure-result "no packageid received"))
    (catch Exception e (do
                         ;; запись в лог
                         (log/error :msg "package 1c processing fail"
                                    :exception e)
                         ;; возврат отриц.результата
                         (failure-result (.getMessage e))))))


(defn- process-package-data
  "Обработать данные пакета"
  [conn config package-data]
  (let [package-number (xml-u/zipper->value package-data :package :PackageNumber)]
    ;; если передан нормально id пакета
    (if package-number
      (try
        ;; доп.проверка на то, что еще не обработали этот пакет
        (if (not (package-already-processed? conn package-number))
          ;; обрабатываем пакет
          (let [process-result (u-s/process-source-with-config conn
                                                               config
                                                               package-data)]
            ;; если не было ошибок при обработке
            (if (= 0 (get-in process-result [:stats :failures]))
              (do
                ;; запись в таблицу пакетов успешную обработку со статами
                (write-package-to-history conn package-number :success)
                ;; возврат успешного результата
                {:type :success})
              (do
                (log/error :msg "package 1c processing fail (see inners)"
                           :package-id package-number)
                (write-package-to-history conn package-number :failure)
                {:type :failure
                 :error "package processing error"})))
          ;; отриц.результат, пакет уже был обработан
          {:type :failure
           :error "package already processed"})
        ;; если возникла ошибка при обработке, то запись в таблицу пакетов
        (catch Exception e (do
                             ;; запись в лог
                             (log/error :msg "package 1c processing fail"
                                        :package-id package-number
                                        :exception e)
                             (write-package-to-history conn package-number :failure)
                             (throw e))))
      ;; выдаем исключение, что id не передано
      (throw (Exception. "no packageid received")))))


(defn process-package
  "Обработка пакета с 1С"
  [conn config xml-body]
  (println "--- post-proccesspackage ---")
  ;; обработка
  (try
    (let [result (open-handlers/open-and-process
                   :xml
                   :http-body
                   xml-body
                   #(process-package-data conn config %)
                   {})]
      (if (= (:type result) :success)
        (success-result)
        ;; иначе обработка неудачная
        (failure-result (:error result))))
    ;; если на каком-либо этапе возникла ошибка
    (catch Exception e (do
                         ;; запись в лог
                         (log/error :msg "package 1c processing critical"
                                    :exception e)
                         ;; возврат отрицательного результата
                         (failure-result (.getMessage e))))))

; (def test-ent
;   {:doc-type-1c "Платежное поручение исходящее",
;    :comment-1c nil,
;    :date #inst "2017-04-24T00:00:00.000-00:00",
;    :v-type :fact,
;    :v-summ 137700.0,
;    :in-number-1c nil,
;    :editable? false,
;    :currency-rate-1c 1.0,
;    :currency-1c "KZT",
;    :number-1c "00000362   ",
;    :v-flow :outflow,
;    :currency-summ-1c 137700.0,
;    :dims [{:name "Сбербанк060441002422",
;            :group-name "Контрагенты",
;            :uuid-1c #uuid "cf25a360-2b14-11e3-8184-00270e03a4fc"}
;           {:name "KZ86914398914BC36670 тенге в Сбербанк2",
;            :group-name "Счета",
;            :uuid-1c #uuid "67531379-24f6-11e3-b503-902b34bf17fe"}],
;    :in-date-str-1c "0001-01-01",
;    :op-type :U,
;    :purpose-1c "16Для зачисления на картсчета сотрудникам согласно спискам 137 700-00 казахстанские тенге      ",
;    :booking-account-1c "1030",
;    :uuid-1c #uuid "daa8264a-28a7-11e7-b426-74d02b9092de"})
;
;
; (defn ttt
;   []
;   (ent->tx-form (m/get-conn) :entry-1c test-ent))
;
; ; (du/field-by-other-field :dimension/name
; ;                          :dimension/uuid-1c #uuid "67531379-24f6-11e3-b503-902b34bf17fe"
; ;                          (m/get-conn))
;
;
; (defn t1
;   []
;   (let [filename "importcsv/second.xml"]
;     (with-open [stream (io/input-stream (-> filename io/file))]
;       (-> stream
;           xml/parse
;           zip/xml-zip
;           (xml-u/zipper->inners :Entry)
;           (#(map (fn [z]
;                    (-> z
;                        xml-u/zipper-into-map
;                        :Entry))
;                  %))))))
;
;
;           ; u/unlazy))))
;
;
; (defn t2
;   []
;   (u-s/parse-item-default
;     {:parse-type :xml
;      :parse-to   :entry-1c
;      :item-tag   :Entry
;      :mappings
;       {:UUID {:display-name "1С UUID записи"
;               :field :uuid-1c
;               :type :uuid}
;        :Operation {:display-name "Тип операции"
;                    :field :op-type
;                    :type :match
;                    :params {:matches {"U" :U
;                                       "D" :D}}}
;        :DocumentType {:display-name "Тип документа"
;                       :field :doc-type-1c
;                       :type :str}
;        :FlowType {:display-name "Тип потока"
;                   :field :v-flow
;                   :type :match
;                   :params {:matches {"inflow" :inflow
;                                      "outflow" :outflow}}}
;        :Number         {:display-name "Рег. номер"
;                         :field :number-1c
;                         :type :str}
;        :Date           {:display-name "Дата"
;                         :field :date
;                         :type :date}
;        :Sum            {:display-name "Сумма"
;                         :field :v-summ
;                         :type :double}
;        :Currency       {:display-name "Валюта"
;                         :field :currency-1c
;                         :type :str}
;        :CurrencyRate   {:display-name "Курс валюты"
;                         :field :currency-rate-1c
;                         :type :double}
;        :SumInCurrency  {:display-name "Сумма в валюте"
;                         :field :currency-summ-1c
;                         :type :double}
;        :BookingAccount {:display-name "Счет учета"
;                         :field :booking-account-1c
;                         :type :str}
;        :IncomingNumber {:display-name "Вх.номер документа"
;                         :field :in-number-1c
;                         :type :str}
;        :IncomingDate   {:display-name "Дата вх.док-та"
;                         :field :in-date-str-1c
;                         :type :str}
;        ; :Organization   {:display-name "Организация"
;        ;                  :field :dims
;        ;                  :type :custom
;        ;                  :params {:group-name "Организация"}}
;                         ;  :params {:nested {:UUID ""}}}
;        :BankAccount    {:display-name "Банковский счет"
;                         :field :dims
;                         :type :custom
;                         :params {:group-name "Счет"}}
;        :Contractor     {:display-name "Контрагент"
;                         :field :dims
;                         :type :custom
;                         :params {:group-name "Контрагенты"}}
;        :Contract       {:display-name "Договор"
;                         :field :dims
;                         :type :custom
;                         :params {:group-name "Договоры"}}
;        :Purpose        {:display-name "Назначение платежа"
;                         :field :purpose-1c
;                         :type :str}
;        :Comment        {:display-name "Комментарий"
;                         :field :comment-1c
;                         :type :str}}
;
;      ;; поля по умолчанию
;      :transform-post {:ent-k :transform-1c-entry
;                       :params {:editable? false
;                                :v-type    :fact}}}
;     ; {:Operation "D",
;     ;  :UUID "0850b08f-fb89-11e3-8255-00270e03a4fc"}
;     {:Operation "U",
;      :Comment "ретрансляция за 4 квартал 2014",
;      :Contract {:UUID "0d195fc3-9fff-11e2-99d1-902b34bf17fe", :Name "№79 от 1/01/2013"},
;      :Purpose nil,
;      :Contractor {:UUID "0d0ee599-525f-453c-b759-2dc23da3dbd8", :Name "RT ТОО040140000515"},
;      :Currency "KZT",
;      :DocumentType "Платежное поручение входящее",
;      :Number "00000000080",
;      :SumInCurrency "10000",
;      :BookingAccount "1030",
;      :BankAccount {:UUID "67531379-24f6-11e3-b503-902b34bf17fe",
;                    :Name "KZ86914398914BC36670 тенге в Сбербанк",}
;      :IncomingNumber "00046.ко",
;      :Sum "10000", :Date "2014-04-17", :FlowType "inflow",
;      :UUID "d1bcface-c92d-11e3-82fc-00270e03a4fc",
;      :IncomingDate "2014-04-17",
;      :Organization {:UUID "b8da4b27-d788-4117-9df1-38f47c9d4aa4",
;                     :Name "Национальный филиал МТРК \"Мир\" в РК",}
;      :CurrencyRate "1"}))
;
; (defn t3
;   []
;   (let [conn (m/get-conn)
;         config  {:id  :processing-1c-xml
;                  :name "Обработка xml от 1С обработчика"
;                  :stages
;                  [{:stage-id          :process-xml
;                    :stage-type        :process-source
;                    :run-when          :always
;                    :source-processing :by-item
;                    :parse-params {:parse-type :xml
;                                   :parse-to   :entry-1c
;                                   :item-tag   :Entry
;                                   :mappings
;                                    {:UUID {:display-name "1С UUID записи"
;                                            :field :uuid-1c
;                                            :type :uuid}
;                                     :Operation {:display-name "Тип операции"
;                                                 :field :op-type
;                                                 :type :match
;                                                 :params {:matches {"U" :U
;                                                                    "D" :D}}}
;                                     :DocumentType {:display-name "Тип документа"
;                                                    :field :doc-type-1c
;                                                    :type :str}
;                                     :FlowType {:display-name "Тип потока"
;                                                :field :v-flow
;                                                :type :match
;                                                :params {:matches {"inflow" :inflow
;                                                                   "outflow" :outflow}}}
;                                     :Number         {:display-name "Рег. номер"
;                                                      :field :number-1c
;                                                      :type :str}
;                                     :Date           {:display-name "Дата"
;                                                      :field :date
;                                                      :type :date}
;                                     :Sum            {:display-name "Сумма"
;                                                      :field :summ
;                                                      :type :double}
;                                     :Currency       {:display-name "Валюта"
;                                                      :field :currency-1c
;                                                      :type :str}
;                                     :CurrencyRate   {:display-name "Курс валюты"
;                                                      :field :currency-rate-1c
;                                                      :type :double}
;                                     :SumInCurrency  {:display-name "Сумма в валюте"
;                                                      :field :currency-summ-1c
;                                                      :type :double}
;                                     :BookingAccount {:display-name "Счет учета"
;                                                      :field :booking-account-1c
;                                                      :type :str}
;                                     :IncomingNumber {:display-name "Вх.номер документа"
;                                                      :field :in-number-1c
;                                                      :type :str}
;                                     :IncomingDate   {:display-name "Дата вх.док-та"
;                                                      :field :in-date-str-1c
;                                                      :type :str}
;                                     ; :Organization   {:display-name "Организация"
;                                     ;                  :field :dims
;                                     ;                  :type :custom
;                                     ;                  :params {:group-name "Организация"}}
;                                                      ;  :params {:nested {:UUID ""}}}
;                                     :BankAccount    {:display-name "Банковский счет"
;                                                      :field :dims
;                                                      :type :custom
;                                                      :params {:group-name "Счета"}}
;                                     :Contractor     {:display-name "Контрагент"
;                                                      :field :dims
;                                                      :type :custom
;                                                      :params {:group-name "Контрагенты"}}
;                                     :Contract       {:display-name "Договор"
;                                                      :field :dims
;                                                      :type :custom
;                                                      :params {:group-name "Договоры"}}
;                                     :Purpose        {:display-name "Назначение платежа"
;                                                      :field :purpose-1c
;                                                      :type :str}
;                                     :Comment        {:display-name "Комментарий"
;                                                      :field :comment-1c
;                                                      :type :str}}
;                                   ;; поля по умолчанию
;                                   :transform-post {:ent-k :transform-1c-entry
;                                                    :params {:editable? false
;                                                             :v-type    :fact}}}}]}
;         source (io/input-stream (-> "importcsv/second.xml" io/file))]
;     (u-s/process-source-with-config conn config source)))
;
;
; (defn zipper->map
;   []
;   (xml-u/zipper->map
;     (xml-u/str->zipper
;       "<Entry>
;     		<UUID>0850b08f-fb89-11e3-8255-00270e03a4fc</UUID>
;     		<Operation>U</Operation>
;     		<DocumentType>Платежное поручение входящее</DocumentType>
;     		<FlowType>inflow</FlowType>
;     		<Number>00000000107</Number>
;     		<Date>2014-06-23</Date>
;     		<Sum>30000</Sum>
;     		<Currency>KZT</Currency>
;     		<CurrencyRate>1</CurrencyRate>
;     		<SumInCurrency>30000</SumInCurrency>
;     		<BookingAccount>1030</BookingAccount>
;     		<IncomingNumber>31</IncomingNumber>
;     		<IncomingDate>2014-06-23</IncomingDate>
;     		<Organization>
;     			<UUID>b8da4b27-d788-4117-9df1-38f47c9d4aa4</UUID>
;     			<Name>Национальный филиал МТРК \"Мир\" в РК</Name>
;     		</Organization>
;     		<BankAccount>
;     			<UUID>67531379-24f6-11e3-b503-902b34bf17fe</UUID>
;     			<Name>KZ86914398914BC36670 тенге в Сбербанк</Name>
;     		</BankAccount>
;     		<Contractor>
;     			<UUID>498d03ea-be27-4e34-a86f-7ae703a66bef</UUID>
;     			<Name>Руд Экспресс 071040000524</Name>
;     		</Contractor>
;     		<Contract>
;     			<UUID>7967d70f-8b8f-11e2-97b3-001d7dd2bcc3</UUID>
;     			<Name>№ 64 01/04/09</Name>
;     		</Contract>
;     		<Purpose/>
;     		<Comment>ретрансляция за 1-3 кварталы 2014</Comment>
;     	</Entry>")
;     [:UUID :Operation :FlowType
;      :Number :Date
;      :Sum :Currency :CurrencyRate :SumInCurrency
;      :BookingAccount :IncomingNumber :IncomingDate
;      :Organization :BankAccount :Contractor
;      :Contract :Purpose :Comment]))
;
;
; (def test-zipper
;   (xml-u/str->zipper
;                   "<Entry>
;                     <UUID>0850b08f-fb89-11e3-8255-00270e03a4fc</UUID>
;                     <Operation>U</Operation>
;                     <DocumentType>Платежное поручение входящее</DocumentType>
;                     <FlowType>inflow</FlowType>
;                     <Number>00000000107</Number>
;                     <Date>2014-06-23</Date>
;                     <Sum>30000</Sum>
;                     <Currency>KZT</Currency>
;                     <CurrencyRate>1</CurrencyRate>
;                     <SumInCurrency>30000</SumInCurrency>
;                     <BookingAccount>1030</BookingAccount>
;                     <IncomingNumber>31</IncomingNumber>
;                     <IncomingDate>2014-06-23</IncomingDate>
;                     <Organization>
;                       <UUID>b8da4b27-d788-4117-9df1-38f47c9d4aa4</UUID>
;                       <Name>Национальный филиал МТРК \"Мир\" в РК</Name>
;                     </Organization>
;                     <BankAccount>
;                       <UUID>67531379-24f6-11e3-b503-902b34bf17fe</UUID>
;                       <Name>KZ86914398914BC36670 тенге в Сбербанк</Name>
;                     </BankAccount>
;                     <Contractor>
;                       <UUID>498d03ea-be27-4e34-a86f-7ae703a66bef</UUID>
;                       <Name>Руд Экспресс 071040000524</Name>
;                     </Contractor>
;                     <Contract>
;                       <UUID>7967d70f-8b8f-11e2-97b3-001d7dd2bcc3</UUID>
;                       <Name>№ 64 01/04/09</Name>
;                     </Contract>
;                     <Purpose/>
;                     <Comment>ретрансляция за 1-3 кварталы 2014</Comment>
;                   </Entry>"))
