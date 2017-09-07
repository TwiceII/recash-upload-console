(ns recash-upload-console.common.xml-utils
  "Функции для работы с xml"
  (:require [clojure.data.xml :as xml]
            [clojure.zip :as zip]
            [clojure.java.io :as io]
            [clojure.data.zip.xml :as zip-xml]
            [recash-upload-console.common.utils :as u]))


(defn hiccup->xml-element
  [hiccup-vector]
  (xml/sexp-as-element hiccup-vector))


(defn hiccup->xml-str
  "Преобразовать hiccup в xml строку"
  [hiccup-vector]
  (xml/emit-str (hiccup->xml-element hiccup-vector)))


(defn parse-string [string]
  (xml/parse-str string))


(defn pre-process-node
  "Заменить :xsi/type на xsi:type внутри нода
  (для норм.проглатывания by emit-str)"
  [xml-node]
  (clojure.walk/postwalk #(if (= % :xsi/type) :xsi:type %)
                         xml-node))


(defn zipper->xml-in-str
  "Получить zipper в xml строке (для логирования и прочего)
  без верхней <?xml version..> и т.д"
  [xml-zipper]
  (-> xml-zipper
      zip/node
      pre-process-node
      xml/emit-str
      (clojure.string/replace "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" "")))
  ;(xml/emit-str (pre-process-node (zip/node xml-zipper))))


(defn zipper->value
  "Получить значение поля из zipper"
  [xml-zipper & fields]
  (when xml-zipper
    (->> fields
         (apply zip-xml/xml1-> xml-zipper)
         (#(when % (zip-xml/text %)))
         (#(if (clojure.string/blank? %) nil %)))))


(defn zipper->inners
  "Получить зипперы для внутренних подтэгов"
  [xml-zipper & inner-tags]
  (apply zip-xml/xml-> xml-zipper inner-tags))


(defn str->zipper
  "Перевести xml строку в зиппер"
  [xml-str]
  (-> xml-str
    java.io.StringReader.
    xml/parse
    zip/xml-zip
    (#(zipper->inners % (get (first %) :tag)))
    first))


(defn str->zipped-source
  "Перевести xml строку в основную зипперную форму"
  [xml-str]
  (-> xml-str
      java.io.StringReader.
      xml/parse
      zip/xml-zip))

(defn zipper->map
  "Преобразовать xml zipper в хэшмэп"
  [zipper fields]
  (reduce (fn [m field]
            (if-let [v (zipper->value zipper field)]
              (assoc m field v)
              m))
          {} fields))


(defn get-value-in-path
  "Получить значение по последнему тэгу в path"
  [z tags]
  (let [tags-v (->> tags
                    (into []))
        path (pop tags-v)]
    (-> z
        (#(apply zipper->inners % path))
        first
        (zipper->value (peek tags-v)))))


(defn zipper-into-map
  "Преобразовать зиппер в мэп со всеми полями
   (поля можно не знать самому)
   предполагается что нет повторяющихся полей в зиппере
   WARNING: работает очень медленно если zipper здоровенный"
  [zipper]
  (->> zipper
       (clojure.walk/postwalk
         (fn [el]
           (if (instance? clojure.data.xml.Element el)
             {(:tag el) (:content el)}
             el)))
       first
       (clojure.walk/postwalk
         (fn [el]
           (if (map? el)
             (reduce-kv (fn [m k v]
                          (assoc m k
                            (if (not (u/nil-or-empty? v))
                              (reduce (fn [rm i]
                                        (if (map? i)
                                          (assoc rm (first (keys i))
                                                    (first (vals i)))
                                          i))
                                      {} v)
                              nil)))
                        {} el)
             el)))))

;;-- работа с потоками --------------------------------------------------------

(defn with-open-stream-to-xml
  "Считать с потока байтов в xml в зиппер-виде
  и отправляет в обрабатывающую функцию

  input-bytes - поток байтов
  process-fn - функция для обработки xml в зиппер-виде"
  [input-bytes process-fn]
  (with-open [stream (io/input-stream input-bytes)]
    (-> stream
        xml/parse
        zip/xml-zip
        process-fn)))


(defn with-open-xml-file
  "Считать файл xml из dev-resources"
  [filename process-fn]
  (-> filename
      io/resource
      io/file
      (with-open-stream-to-xml process-fn)))


(defn with-open-local-xml-file
  "Считать файл xml из dev-resources"
  [filename process-fn]
  (-> filename
      io/file
      (with-open-stream-to-xml process-fn)))

; (-> "importcsv/first.xml"
;     io/file
;     (with-open-stream-to-xml (fn [x]
;                                (println x))))
;
; (with-open-xml-file
;   "importcsv/first.xml"
;   (fn [x]
;     (println x)))

;; (with-open-xml-file
;;   "primer.xml" (fn [_] (println "Hey there")))


;; (hiccup->xml-str [:result {}
;;                   [:status {} "success"]])

;; (hiccup->xml-str [:result {}
;;                   [:status {} "failure"]
;;                   [:error {} "errrrr"]])

;; (let [testdata (dev-resources-file->xml-zip "testpackage.xml")]
;;   (zipper->value testdata :package :packageNumber))
