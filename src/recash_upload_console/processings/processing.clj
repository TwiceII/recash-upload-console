(ns recash-upload-console.processings.processing
  (:require [recash-upload-console.processings.actions :as act]
            [recash-upload-console.common.utils :as u]
            [io.pedestal.log :as log]))


(defn process-stage-with-logs
  "Обработка этапа с записью в лог, ловлей исключений и т.д."
  [conn source stage-config]
  (println "processing stage: " (:name stage-config))
  (println "action-params: " (:action stage-config))
  (try
    (let [stage-output (act/run-action-on-source conn source (:action stage-config))]
      (do
        ;; запись в лог удачной обработки этапа
        (log/info :msg (str "process stage SUCCESS: " (:id stage-config)))
        {:result :success
         :data   stage-output}))
    (catch Exception e (do
                         ;; запись в лог неуд.обработки этапа
                         (log/error :msg (str "process stage FAIL: " (:id stage-config))
                                    :exception e)
                         {:result :failure
                          :exception e}))))


(defn process-source-with-config
  "Главная функция обработки источника с помощью конфига с этапами"
  [conn source config]
  (println "-----------------------")
  (println "processing config: " (:name config))
  (log/info :msg (str "Processing config: " (:id config)))
  (u/info-map m
    ;; результаты каждого этапа
    :stage-results
      (reduce (fn [results-map stage-config]
                (assoc results-map (:id stage-config)
                   (let [run-when (:run-when stage-config)]
                    ;; если запускать всегда
                    (if (= :always run-when)
                      ;; запускаем обработку этапа
                      (process-stage-with-logs conn source stage-config)
                      ;; иначе проверяем по предыдущим этапам
                      (let [stages-to-check (if (vector? run-when)
                                              (-> results-map
                                                  (select-keys run-when)
                                                  vals)
                                              ;; если no-errors
                                              (case run-when
                                                :no-errors (vals (results-map))))]
                        (if (every? #(= :success (:result %)) stages-to-check)
                          ;; запускаем обработку этапа
                          (process-stage-with-logs conn source stage-config)
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
