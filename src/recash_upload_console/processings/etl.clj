(ns recash-upload-console.processings.etl
    (:require [recash-upload-console.processings.loading :as l]
              [recash-upload-console.processings.parsing-and-mapping :as pnm]
              [recash-upload-console.processings.validating :as vld]
              [recash-upload-console.common.utils :as u]
              [recash-upload-console.processings.actions
               :refer [run-action-on-source]]
              [io.pedestal.log :as log]))


(defn run-etl-item
  "Общий метод для ETL обработки отдельного элемента из источника"
  [conn item pnm-params load-params]
  (println "start process item: " item)
  (try
    (let [mapped-item-type (get-in pnm-params [:mapping :to])]
      (-> (u/info-map m
            :item           item
            :mapped-item    (pnm/item->mapped-item (:item m)
                                                   pnm-params)
            :valid-errors   (vld/validate-mapped-item conn
                                                      (:mapped-item m)
                                                      mapped-item-type)
            :load-item      (let [errors (:valid-errors m)]
                              (when (u/nil-or-empty? errors) ;; если нет ошибок
                                (l/ent->load-item conn
                                                  (:mapped-item m)
                                                  mapped-item-type
                                                  (:type load-params))))
            :process-results (if-not (u/nil-or-empty? (:valid-errors m))
                               (do
                                 (println "process-item validation fail: ")
                                 ;; пишем в логи, возвр. неудачный результат
                                 (log/info :msg (str "process-item FAIL (validation)")
                                           :item item
                                           :parsed-item (:mapped-item m)
                                           :valid-errors (:valid-errors m))
                                 :failure)
                               (do
                                 ;; если не игнорируем запись
                                 (if-not (= :ignore (:load-item m))
                                  (do
                                    ;; пишем в логи
                                    (log/info :msg (str "process-item SUCCESS")
                                              :item item)
                                    ; (println "txs: " (:txs m))
                                    (l/process-load-item conn
                                                         (:load-item m)
                                                         (:type load-params)))
                                  ;; игнорируем
                                  (do
                                    (println "process item ignore")
                                    ; ;; пишем в логи
                                    (log/info :msg (str "process-item IGNORE")
                                              :item item)))
                                 :success)))
          (#(-> [(:item %) (:load-item %) (:process-results %)]))))
    (catch Exception e (do
                        ;; запись в логи
                        (println "Exception on process item")
                        (throw e)))))


(defmethod run-action-on-source :etl-by-items
  [conn source action-params]
  (let [{:keys [pnm-params load-params]} action-params]
    (try
      ;; без with-open
      (->> (pnm/source->items source pnm-params)
           (map #(run-etl-item conn % pnm-params load-params))
           doall)
      (catch Exception e (do
                          (println "e on run-action-on-source :etl-by-items " e)
                          (throw e))))))
