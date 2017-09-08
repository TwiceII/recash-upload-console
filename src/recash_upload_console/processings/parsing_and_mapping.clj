(ns recash-upload-console.processings.parsing-and-mapping
  (:require [recash-upload-console.common.utils :as u]
            [recash-upload-console.common.xml-utils :as xml-u]
            [recash-upload-console.common.parsing-utils :as pu]))


;; -- стандартные парсеры полей -----------------------------------------------
(defmulti v-by-type
  "Парсинг по типу поля (стандартный)"
  (fn [v params type] type))

(defn match-from-mapping
  [match-params v]
  (pu/str->match match-params v))

(defmethod v-by-type :str
  [v params type]
  v)

(defmethod v-by-type :uuid
  [v params type]
  (java.util.UUID/fromString v))

(defmethod v-by-type :double
  [v params type]
  (pu/str->double v))

(defmethod v-by-type :date
  [v params type]
  (pu/entry-date-str->jdate v))

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

(defmulti item->field-value
  "Получение значение поля из элемента"
  (fn [item field parse-from] parse-from))

(defmethod item->field-value :xml
  [item field parse-from]
  (get item field))

(defmethod item->field-value :csv
  [item field parse-from]
  (nth item field))


;; -- Мапинг элемента ---------------------------------------------------------
(defmulti ignore-mapping?
  "Признак, что нужно игнорировать элемент и не делать mapping"
  (fn [item pred-key] pred-key))


(defmulti map-item-via
  "Получить mapped-item через метод"
  (fn [item via-params item-parsed-from] (:method via-params)))


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
                   (v-by-type params by-type)))
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


(defn item->mapped-item
  "Общий метод получения mapped-item из item"
  [item pnm-params]
  (let [item-parsed-from (get-in pnm-params [:parse :from])
        mapping-params   (:mapping pnm-params)
        ignore-when      (:ignore-when mapping-params)
        via-params       (:via mapping-params)]
    (-> (u/info-map m
          :item item
          :ignore-mapping? (if ignore-when
                             (ignore-mapping? item ignore-when)
                             false)
          :mapped-item (if-not (:ignore-mapping? m)
                         (map-item-via item via-params item-parsed-from)
                         :ignore))
        :mapped-item)))




;; -- Получение элементов из источника ----------------------------------------
(defmulti source->items
  "Получить элементы (строки и т.д.) из источника"
  (fn [source pnm-params] (:parse-type pnm-params)))


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
  (let [e-tag        (get-in pnm-params [:parse :item-params :tag])
        inner-fields (get-in pnm-params [:parse :item-params :inner-fields])
        ;; предполагается, что xml/parse и zip/xml-zip делались выше
        zipped-source source]
    (-> zipped-source
        (xml-u/zipper->inners e-tag)
        (#(map (fn [z] (zipper->item z inner-fields))
               %)))))
