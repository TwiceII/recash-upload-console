(ns recash-upload-console.schedulers.chime-scheduler
  (:require [recash-upload-console.common.utils :as u]
            [io.pedestal.log :as log]
            [clj-time.core :as t]
            [clj-time.periodic :refer [periodic-seq]]
            [recash-upload-console.schedulers.ummastore-schedule
             :refer [sync-with-ummastore]]
            [chime :refer [chime-at]]))


(defn common-handler
  "Общий обработчик заданий с логированием"
  [schedule-time handler-fn postfix settings]
  (try
    ; (log/log-scheduler-start schedule-time postfix)
    (handler-fn settings schedule-time)
    (catch Exception e (do
                         (println "exception on scheduler: " postfix)
                         (println e)))))
                        ;  (log/log-exception e "error on scheduler")
                        ;  (log/log-scheduler-exception schedule-time
                        ;                               postfix
                        ;                               e)))))

(defn schedule-handler
  "Обработчик заданий"
  [handler-fn postfix settings]
  (fn [schedule-time]
    (common-handler schedule-time handler-fn postfix settings)))


(defn start-schedule!
  "Запустить задание"
  [sch-hm settings]
  (chime-at (:schedule sch-hm)
            (schedule-handler (:task-fn sch-hm)
                              (:postfix sch-hm)
                              settings)))


;;=========================================================================
; список заданий по расписанию
(def all-schedules
  [{:postfix "ummastore-sync"
    :schedule (periodic-seq (t/now) ; каждые 10 секунд
                            (-> 10 t/seconds))
    :task-fn sync-with-ummastore}])


(defn start-all-schedules!
  "Запустить все задания по расписанию"
  [settings]
  (println "start-all-schedules")
  (doseq [s all-schedules]
    (do
      (println "started " (:postfix s))
      (start-schedule! s settings))))
