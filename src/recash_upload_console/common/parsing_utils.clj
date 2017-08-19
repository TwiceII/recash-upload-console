(ns recash-upload-console.common.parsing-utils
  "Функции для парсинга значений"
  (:require [recash-upload-console.common.utils :as u]
            [recash-upload-console.common.time-utils :as tu]
            [clojure.string :as cljstr]))


(defn str->double
  [v]
  (-> v
      (cljstr/trim)
      (cljstr/replace " " "")
      (cljstr/replace "," ".")
      (Double/parseDouble)))


(defn str->match
  [match-params v]
  (if-let [match-v (get match-params v)]
    match-v
    (throw (Exception. (str "Не найдено соответствие для " v)))))


(defn entry-date-str->jdate
  [x]
  (-> x
      ; (cljstr/replace " 0:00:00" "")
      tu/parse-to-jdate))
