(ns recash-upload-console.data-upload.upload-service
  "Функции для загрузки данных из различных сервисов"
  (:require [recash-upload-console.common.utils :as u]
            [recash-upload-console.domain.model :as m]
            [recash-upload-console.domain.datomic-utils :as du]
            [recash-upload-console.common.time-utils :as tu]
            [recash-upload-console.common.parsing-utils :as pu]
            [datomic.api :as d]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as cljstr]
            [clojure.edn :as edn]
            [clojure.spec :as s]
            [io.pedestal.log :as log]
            [recash-upload-console.common.xml-utils :as xml-u]
            [com.stuartsierra.component :as component]))

;; -- Парсинг строковых значений в нужные поля нужной сущности ----------------
(defmulti parse-by-type
  "Парсинг по типу поля (стандартный)"
  (fn [mapping-params v] (:type mapping-params)))

(defmulti parse-custom
  "Кастомный парсинг строкового значения"
  (fn [ek mapping-params v] [ek (:field mapping-params)]))

(defmulti transform-pre
  "Преобразование строки до парсинга"
  (fn [ek transform-params parsed-item] ek))

(defmulti transform-post
  "Преобразование уже отпарсенной униф.записи"
  (fn [ek transform-params parsed-item] ek))

;; метод для добавления значений по умолчанию
(defmethod transform-post :add-defaults
  [ek transform-params parsed-item]
  (merge parsed-item transform-params))


;; -- стандартные парсеры полей -----------------------------------------------
(defn match-from-mapping
  [mapping-params v]
  (pu/str->match (get-in mapping-params [:params :matches]) v))

(defmethod parse-by-type :str
  [mapping-params v]
  v)

(defmethod parse-by-type :uuid
  [mapping-params v]
  (java.util.UUID/fromString v))

(defmethod parse-by-type :double
  [mapping-params v]
  (pu/str->double v))

(defmethod parse-by-type :date
  [mapping-params v]
  (pu/entry-date-str->jdate v))

(defmethod parse-by-type :match
  [mapping-params v]
  (match-from-mapping mapping-params v))



;; -- Парсинг строк в униф.сущность -------------------------------------------

(defmulti get-field-from-item
  "Получить из item значение поля для парсинга"
  (fn [parse-type item field-id]
    parse-type))

(defmethod get-field-from-item :csv
  [parse-type item field-id]
  (nth item field-id))

(defmethod get-field-from-item :xml
  [parse-type item field-id]
  (get item field-id))


(defn many-fields-in-mappings
  "Получить сет всех to-many поля, которые повторяются в маппингах"
  [mapping-params]
  (->> mapping-params
       vals
       (map :field)
       frequencies
       (filter (fn [[k c]] (> c 1)))
       (map first)
       (into #{})))


(defn parse-item-default
  "По умолчанию ф-ия парсинга строки/элемента/записи в унифиц.сущность"
  [parse-params item]
  (let [parse-type (:parse-type parse-params)
        ent-k      (:parse-to parse-params)]
    (-> (u/info-map m
          :item item
          ;; пре-трансформация (если есть)
          :pre-transformed (if-let [tr-pre (:transform-pre parse-params)]
                             (transform-pre (:ent-k tr-pre) (:params tr-pre) (:item m))
                             (:item m))
          ;; парсинг полей
          :parse-fields
            (let [many-fields (many-fields-in-mappings (:mappings parse-params))]
              (reduce-kv (fn [ent-m field-id mapping-params]
                           (let [field      (:field mapping-params)
                                 ;; получаем значение поля для парсинга
                                 v-to-parse (get-field-from-item parse-type
                                                                 (:pre-transformed m)
                                                                 field-id)
                                 ;; парсим значение
                                 value      (when-not (or (u/nil-or-empty? v-to-parse)
                                                          (cljstr/blank? (str v-to-parse)))
                                              (if-not (= :custom (:type mapping-params))
                                                ;; парсим стандартно
                                                (parse-by-type mapping-params v-to-parse)
                                                ;; парсим кастомно (должен быть defmethod)
                                                (parse-custom ent-k
                                                              mapping-params
                                                              v-to-parse)))]
                             (if (contains? many-fields field) ;; если to-many поле
                               (update ent-m field
                                  (fn [pv]
                                    (-> pv
                                        ;; если еще не было ни одного элемента, создаем список
                                        (#(if-not (some? %) [] %))
                                        ;; добавляем если не-nil в список
                                        (#(if (some? value) (conj % value) %)))))
                                ;; если простое поле
                               (assoc ent-m field value))))
                         {} (:mappings parse-params)))
          ;; пост-трансформация (если есть)
          :post-transformed
            (if-let [tr-post (:transform-post parse-params)]
              (transform-post (:ent-k tr-post) (:params tr-post) (:parse-fields m))
              (:parse-fields m)))
        ;; проверяем конечный результат на соответствие spec и возвращаем либо выдаем исключение
        :post-transformed)))
        ; (#(s/assert ent-k (:post-transformed %))))))


(defmulti validate-ent
  "Валидация отпарсенной унифиц.сущности"
  (fn [conn ek e] ek))


(defmulti ent->tx-form
  "Получение транзакц.вида из униф.сущности"
  (fn [conn ek e] ek))


;; -- получить элементы для парсинга из источника -----------------------------
(defmulti get-items-from-source
  "Получить коллекцию элементов из источника
   stage-config - конфиг этапа
   source - файл, поток байтов или другое представление источника"
  (fn [stage-config source]
    (get-in stage-config [:parse-params :parse-type])))


(defmethod get-items-from-source :csv
  [stage-config source]
  source)

;; получить c xml -------------

(defn zipper->item
  [zipper inner-fields]
  (reduce (fn [m inner-field]
            ;; если вложенное поле
            (if (vector? inner-field)
              (let [[main-tag inner-tags] inner-field]
                (assoc m main-tag
                  ;; если такой тег вообще есть
                  (if (first (xml-u/zipper->inners zipper main-tag))
                    (reduce (fn [im itag]
                              (assoc im itag (xml-u/get-value-in-path zipper [main-tag itag])))
                            {} inner-tags))))
              ;; иначе просто напрямую
              (assoc m inner-field (xml-u/zipper->value zipper inner-field))))
          {} inner-fields))


(defmethod get-items-from-source :xml
  [stage-config source]
  (let [e-tag        (get-in stage-config [:parse-params :item-tag])
        inner-fields (get-in stage-config [:parse-params :inner-fields])
        ;; предполагается, что xml/parse и zip/xml-zip делались выше
        zipped-source source]
    (-> zipped-source
        (xml-u/zipper->inners e-tag)
        (#(map (fn [z] (zipper->item z inner-fields))
               %)))))
;; --------------

;; -- Обработка сущностей из источника ----------------------------------------
(defn process-item-default
  "Общий метод для обработки отдельного элемента из источника"
  [conn stage-config item]
  (println "start process item: " item)
  (try
    (let [ent-k (get-in stage-config [:parse-params :parse-to])]
      (-> (u/info-map m
            :item           item
            :parsed-item    (parse-item-default (:parse-params stage-config)
                                                (:item m))
            :valid-errors   (validate-ent conn ent-k (:parsed-item m))
            :txs            (let [errors (:valid-errors m)]
                              (if-not (u/nil-or-empty? errors) ;; если есть ошибки
                                :none
                                ;; иначе переводим в транз.форму
                                (ent->tx-form conn ent-k (:parsed-item m))))
            :process-results (if-not (u/nil-or-empty? (:valid-errors m))
                               (do
                                 (println "process-item validation fail: ")
                                 ;; пишем в логи, возвр. неудачный результат
                                 (log/info :msg (str "process-item FAIL (validation)")
                                           :item item
                                           :parsed-item (:parsed-item m)
                                           :valid-errors (:valid-errors m))
                                 :failure)
                               (do
                                 ;; если не игнорируем запись
                                 (if-not (= :ignore (:txs m))
                                  (do
                                    ;; пишем в логи
                                    (log/info :msg (str "process-item SUCCESS")
                                              :item item)
                                    ; (println "txs: " (:txs m))
                                    (du/transact-and-return conn (:txs m)))
                                  ;; игнорируем
                                  (do
                                    (println "process item ignore")
                                    ; ;; пишем в логи
                                    (log/info :msg (str "process-item IGNORE")
                                              :item item)))
                                 :success)))
          (#(-> [(:item %) (:txs %) (:process-results %)]))))
    (catch Exception e (do
                        ;; запись в логи
                        (println "Exception on process item")
                        (throw e)))))



;; -- Обработка источника -----------------------------------------------------
(defn process-source-by-items-default
  "Обработка по умолчанию внешнего источника построчно/поэлементно
   с with-open"
  [conn stage-config source]
  (let [ent-k (get-in stage-config [:parse-params :parse-to])]
    (try
      ;; без with-open
      (->> (get-items-from-source stage-config source)
           (map #(process-item-default conn stage-config %))
           doall)
      (catch Exception e (do
                          (println "e on process-source-by-items-default: " e)
                          (throw e))))))

(defmulti process-source
  "Обработка источника"
  (fn [conn stage-config source]
    [(get-in stage-config [:parse-params :parse-type])
     (:source-processing stage-config)]))


(defmethod process-source [:csv :by-item]
  [conn stage-config source]
  (process-source-by-items-default conn stage-config source))


(defmethod process-source [:xml :by-item]
  [conn stage-config source]
  (process-source-by-items-default conn stage-config source))


(defmulti process-self
  "Обработка встроенных в конфиг данных"
  (fn [conn stage-config] (:process-ent stage-config)))


;; -- Обработка этапов --------------------------------------------------------
(defmulti process-stage
  "Обработка этапа из конфига"
  (fn [conn stage-config source] (:stage-type stage-config)))

(defmethod process-stage :process-self
  [conn stage-config source]
  (process-self conn stage-config))

(defmethod process-stage :process-source
  [conn stage-config source]
  (process-source conn stage-config source))


(defn process-stage-with-logs
  "Обработка этапа с записью в лог, ловлей исключений и т.д."
  [conn stage-config source]
  (println "processing stage: " (:stage-id stage-config))
  (try
    (let [process-output (process-stage conn stage-config source)]
      (do
        ;; запись в лог удачной обработки этапа
        (log/info :msg (str "process stage SUCCESS: " (:stage-id stage-config)))
        {:result :success
         :data   process-output}))
    (catch Exception e (do
                         ;; запись в лог неуд.обработки этапа
                         (log/error :msg (str "process stage FAIL: " (:stage-id stage-config))
                                    :exception e)
                         {:result :failure
                          :exception e}))))


(defn process-source-with-config
  "Применить конкретную обработку к источнику данных (файл, данные по каналу и т.д.)"
  [conn config source]
  (println "-----------------------")
  (println "processing config: " (:id config))
  (log/info :msg (str "Processing config: " (:id config)))
  (u/info-map m
    ;; результаты
    :stage-results
      (reduce (fn [results-map stage-config]
                (assoc results-map (:stage-id stage-config)
                   (let [run-when (:run-when stage-config)]
                    (if (= :always run-when)
                      (process-stage-with-logs conn stage-config source)
                      ;; иначе проверяем по предыдущим этапам
                      (let [stages-to-check (if (vector? run-when)
                                              (-> results-map
                                                  (select-keys run-when)
                                                  vals)
                                              ;; если no-errors
                                              (case run-when
                                                :no-errors (vals (results-map))))]
                        (if (every? #(= :success (:result %)) stages-to-check)
                          (process-stage-with-logs conn stage-config source)
                          {:result :did-not-run}))))))
              {} (:stages config))
    ;; статистика
    :stats
      (let [count-by-result-fn (fn [rk] (->> (:stage-results m)
                                             vals
                                             (filter #(= rk (:result %)))
                                             count))]
        {:total       (count (:stages config))
         :successes   (count-by-result-fn :success)
         :failures    (count-by-result-fn :failure)
         :did-not-run (count-by-result-fn :did-not-run)})))
