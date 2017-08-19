(ns recash-upload-console.common.open-handlers
  "Функции для открытия потоков/файлов и т.д."
  (:require [recash-upload-console.common.utils :as u]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as cljstr]
            [clojure.edn :as edn]
            [recash-upload-console.common.xml-utils :as xml-u]
            [clojure.data.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as zip-xml]))


;; -- открытие потока ---------------------------------------------------------
(defmulti get-stream-to-open
  (fn [src-type open-type src params] [src-type open-type]))

(defmethod get-stream-to-open [:xml :local-file]
  [src-type open-type src params]
  ;; как src выступает название локал.файла
  (println "src: " src)
  (let [filename src]
    (-> filename
        ; io/resource
        io/file
        io/input-stream)))

(defmethod get-stream-to-open [:xml :http-body]
  [src-type open-type src params]
  ;; как src выступает :body с запроса, уже в виде потока байтов
  ;; поэтому передаем напрямую
  src)

(defmethod get-stream-to-open [:csv :local-file]
  [src-type open-type src params]
  ;; как src выступает название локал.файла
  (io/reader src))


;; -- чтение с потока значений/строк ------------------------------------------
(defmulti read-in-open
  "Метод для чтения строк внутри with-open"
  (fn [src s-type params] s-type))

(defmethod read-in-open :csv
  [src s-type params]
  (csv/read-csv src :separator (or (:separator params)
                                   (first ";"))))

(defmethod read-in-open :xml
  [src s-type params]
  (-> src
      xml/parse
      zip/xml-zip))


;; -- главные функции ---------------------------------------------------------
(defn do-with-open
  "Проделать действия с иточником внутри with-open"
  [src-type open-type src process-fn params]
  (with-open [s (get-stream-to-open src-type open-type src params)]
    (-> s
        (read-in-open src-type params)
        process-fn)))

(defmulti open-and-process
  "Открыть файл/поток/что-либо и обработать"
  (fn [src-type open-type src process-fn params] [src-type open-type]))

(defmethod open-and-process [:xml :local-file]
  [src-type open-type src process-fn params]
  (do-with-open src-type open-type src process-fn params))

(defmethod open-and-process [:xml :http-body]
  [src-type open-type src process-fn params]
  (do-with-open src-type open-type src process-fn params))

(defmethod open-and-process [:csv :local-file]
  [src-type open-type src process-fn params]
  (do-with-open src-type open-type src process-fn params))
