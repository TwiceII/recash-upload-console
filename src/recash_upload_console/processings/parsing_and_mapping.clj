(ns recash-upload-console.processings.parsing-and-mapping
  (:require [recash-upload-console.common.utils :as u]
            [recash-upload-console.common.xml-utils :as xml-u]
            [recash-upload-console.common.parsing-utils :as pu]
            [clojure.data.json :as json]))


;; -- стандартные парсеры полей -----------------------------------------------
(defmulti v-by-type
  "Парсинг по типу поля (стандартный)"
  (fn [v params type] type))

(defn match-from-mapping
  [match-params v]
  (pu/str->match match-params v))

(defmethod v-by-type :str
  [v params type]
  (str v))

(defmethod v-by-type :uuid
  [v params type]
  (java.util.UUID/fromString v))

(defmethod v-by-type :double
  [v params type]
  (if (double? v)
    v
    (pu/str->double v)))

(defmethod v-by-type :date
  [v params type]
  (pu/entry-date-str->jdate v))

(defmethod v-by-type :datetime
  [v params type]
  (pu/entry-datetime-str->jdate v))

(defmethod v-by-type :match
  [v params type]
  (match-from-mapping params v))


;; -- Кастомные методы для получения поля из item
(defmulti item->custom->v
  "Кастомное получение поля из item"
  (fn [item custom-params custom-type item-parsed-from] custom-type))


(defmulti item-has-field?
  "Проверка, что у элемента есть такое поле"
  (fn [item field parse-from] parse-from))

(defmethod item-has-field? :xml
  [item field parse-from]
  (contains? item field))

(defmethod item-has-field? :csv
  [item field parse-from]
  (<= field (count item)))

(defmethod item-has-field? :json
  [item field parse-from]
  (contains? item field))

(defmulti item->field-value
  "Получение значение поля из элемента"
  (fn [item field parse-from] parse-from))

(defmethod item->field-value :xml
  [item field parse-from]
  (get item field))

(defmethod item->field-value :csv
  [item field parse-from]
  (nth item field))

(defmethod item->field-value :json
  [item field parse-from]
  (get item field))

;; -- Мапинг элемента ---------------------------------------------------------
(defmulti ignore-mapping?
  "Признак, что нужно игнорировать элемент и не делать mapping"
  (fn [item pred-key] pred-key))


(defmulti map-item-via
  "Получить mapped-item через метод"
  (fn [item via-params item-parsed-from] (:method via-params)))

(defmulti item->pre-mapped
  "Предобработка элемента до мэппинга"
  (fn [item pre-kw] pre-kw))

(defmulti mapped-item->post
  "Постобработка mapped-item-а"
  (fn [mapped-item post-kw] post-kw))


(defn field-via-mpvector
  "Получить значение поля через mpvector параметры"
  [item mpvector item-parsed-from]
  (let [m-type (first mpvector)]
    (case m-type
      ;; берем константное значение
      :const (let [[_ val] mpvector]
               val)
      ;; считываем поле из источника и парсим по типу
      :field (let [[_ field by-type params] mpvector]
               (-> item
                   ;; если :_this
                   (#(if (= :_this field)
                       %                ; то берем само значение,
                       ;; есло вложенное поле
                       (if (item-has-field? % field item-parsed-from)
                         (item->field-value % field item-parsed-from)
                         (throw (Exception. (str "Not found field: " field " in item: " item))))))
                   ;; парсим по полю
                   (#(when % ; если есть значение
                       (v-by-type % params by-type)))))
      ;; кастомное считывание
      :custom (let [[_ custom-type custom-params] mpvector]
                (item->custom->v item
                                 custom-params
                                 custom-type
                                 item-parsed-from)))))


(defmethod map-item-via :mpvectors
  [item via-params item-parsed-from]
  (let [field-mpvectors (:params via-params)]
    (reduce-kv (fn [nm fk mpvector]
                 (assoc nm fk (field-via-mpvector item
                                                  mpvector
                                                  item-parsed-from)))
               {} field-mpvectors)))

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



(defn item->mapped-item
  "Общий метод получения mapped-item из item"
  [item pnm-params]
  (let [item-parsed-from (get-in pnm-params [:parse :from])
        mapping-params   (:mapping pnm-params)
        ignore-when      (:ignore-when mapping-params)
        pre-mapping      (:pre-mapping mapping-params)
        post-mapping     (:post-mapping mapping-params)
        via-params       (:via mapping-params)]
    (-> (u/info-map m
          :item item
          :ignore-mapping? (if ignore-when
                             (ignore-mapping? item ignore-when)
                             false)
          :mapped-item (if-not (:ignore-mapping? m)
                         (cond-> item
                                 (some? pre-mapping) (item->pre-mapped pre-mapping)
                                 true (map-item-via via-params item-parsed-from)
                                 (some? post-mapping) (mapped-item->post post-mapping))
                         :ignore))
        :mapped-item)))




;; -- Получение элементов из источника ----------------------------------------
(defmulti source->items
  "Получить элементы (строки и т.д.) из источника"
  (fn [source pnm-params] (get-in pnm-params [:parse :from])))


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


(defmethod source->items :xml
  [source pnm-params]
  (println "source->items :xml")
  (let [e-tag        (get-in pnm-params [:parse :item-params :tag])
        inner-fields (get-in pnm-params [:parse :item-params :inner-fields])
        ;; предполагается, что xml/parse и zip/xml-zip делались выше
        zipped-source source]
    (-> zipped-source
        (xml-u/zipper->inners e-tag)
        (#(map (fn [z] (zipper->item z inner-fields))
               %)))))


(defmethod source->items :json
  [source pnm-params]
  (println "souce->items :json")
  (let [e-tag (get-in pnm-params [:parse :item-params :tags])]
    (-> source
        (json/read-str :key-fn keyword)
        (get e-tag))))

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
;       :ignore-when :entry-date-before-2017?
;       :post-mapping :check-op-type-st-entry
;       :via {:method :mpvectors
;             :params
;              {:source/name        [:const "ummastore"]
;               :source/frgn-str-id [:field :id :str]
;               :st-entry/op-type   [:field :opType :match {"U" :U  "D" :D}]
;               :st-entry/date      [:field :date :date]
;               :st-entry/summ      [:field :summ :double]
;               :st-entry/v-flow    [:field :flowType :match {"1" :inflow "0" :outflow}]
;               :st-entry/v-type    [:const :fact]
;               :st-entry/editable? [:const false]
;               :st-entry/dims      [:custom
;                                    :st-entry-dims
;                                    {:defaults {:source/name        [:const "ummastore"]
;                                                :source/frgn-str-id [:field :_this :str]
;                                                :st-dim/type        [:const :must-pre-exist]
;                                                :st-dim/editable?   [:const false]}
;                                     :dims {:client  {:st-dim/group-name [:const "Клиенты"]}
;                                            :account {:st-dim/group-name [:const "Контрагенты"]}
;                                            :store   {:st-dim/group-name [:const "Магазины"]}}}]}}}})
;
;
; (source->items
;   json-str
;   json-pnm-params)
