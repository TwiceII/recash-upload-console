(ns recash-upload-console.schedulers.ummastore-schedule
  "Функции для синхронизации с ummastore"
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [recash-upload-console.domain.model :as md]
            [recash-upload-console.common.utils :as u]
            [recash-upload-console.domain.db-manager :as dbm]
            [recash-upload-console.common.time-utils :as tu]
            [recash-upload-console.processings.processing :as prcs]
            [recash-upload-console.data-upload.upload-dict-dim]
            [recash-upload-console.data-upload.upload-standard-entry]))

(def sync-uri "http://localhost:8890/testjson")

(defn sync-with-ummastore
  "Основная ф-ия синхронизации с ummastore"
  [settings sync-time]
  (println "Hey, we synced with ummastore")
  (println "sync-time: " sync-time)
  (println "settings: " settings)
  (let [db-uri (:db-uri settings)
        conn (dbm/new-conn db-uri)
        config (-> settings
                   :processing-configs-path
                   slurp
                   edn/read-string
                   :processing-ummastore-json)]
    (-> (u/info-map m
          ;; получить последнее время удачной обработки в качестве первой даты,
          ;; если ее нет (в первый раз) - создаем за последний час (или лучше с начала времен?)
          :from-datetime (or (md/last-successfull-datetime-ummastore-sync conn)
                             (tu/starting-jdate-for-sync))
          ;; второй датой сделать текущее время
          :to-datetime (tu/now-jdate)
          ;; создать GET запрос на ummastore с двумя датами
          :full-uri (str sync-uri "?from=" (tu/jdate->iso-str (:from-datetime m))
                                  "&to=" (tu/jdate->iso-str (:to-datetime m)))
          ;; получить json данные
          :source-from-uri (slurp (:full-uri m))
          ;; обработка данных
          :processing (try
                        [:success (prcs/process-source-with-config conn
                                                                   (:source-from-uri m)
                                                                   config)]
                        (catch Exception e (do
                                             (println e)
                                             [:failure])))
          ;; записать последнюю обработанную дату со статусом
          :new-last-datetime (md/new-last-datetime-ummastore-sync conn
                                                                  (:to-datetime m)
                                                                  (first (:processing m))))
        ;; вывести на экран
        (#(println %)))))
