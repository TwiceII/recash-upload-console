(ns recash-upload-console.schedulers.ummastore-schedule
  "Функции для синхронизации с ummastore"
  (:require [clojure.data.json :as json]
            [recash-upload-console.domain.model :as md]
            [recash-upload-console.common.utils :as u]
            [recash-upload-console.domain.db-manager :as dbm]
            [recash-upload-console.common.time-utils :as tu]))

(def sync-uri "http://localhost:8890/testjson")

(defn sync-with-ummastore
  "Основная ф-ия синхронизации с ummastore"
  [settings sync-time]
  (println "Hey, we synced with ummastore")
  (println "sync-time: " sync-time)
  (let [db-uri (:db-uri settings)
        conn (dbm/new-conn db-uri)]
    (-> (u/info-map m
          :from-datetime (or (md/last-successfull-datetime-ummastore-sync conn)
                             (tu/starting-jdate-for-sync))
          :to-datetime (tu/now-jdate)
          :full-uri (str sync-uri "?from=" (tu/jdate->iso-str (:from-datetime m))
                                  "&to=" (tu/jdate->iso-str (:to-datetime m)))
          :json-from-uri
                    (json/read-str (slurp (:full-uri m))
                                   :key-fn keyword))
        (#(println %)))))



; получить последнее время удачной обработки в качестве первой даты,
; если ее нет (в первый раз) - создаем за последний час (или лучше с начала времен?)
; второй датой сделать текущее время
; отправить get запрос на ummastore с двумя датами
; получить json данные, обработать их
; при удачной обработке - записать в БД последней датой вторую
; при неуспешной - записать в БД вторую дату неуспешной

; (json/read-str (slurp "http://localhost:8890/testjson?from=2017-09-13T00:00:00&to=2017-09-14T00:00:00")
;                :key-fn keyword)
