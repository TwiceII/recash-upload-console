(ns recash-upload-console.commands
  "Команды, доступные в консольном приложении"
  (:require [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as cljstr]
            [recash-upload-console.domain.db-manager :as dbm]
            [recash-upload-console.domain.datomic-utils :as du]
            [recash-upload-console.data-upload.processing-manager :as pm]
            [recash-upload-console.schedulers.chime-scheduler :as chime]))

;; -- Команды -----------------------------------------------------------------
(defn exit-cmd
  "Команда для выхода из консоли"
  [_]
  (println "Exiting...")
  (System/exit 0))


(defn start-schedules-cmd
  "Команда для начала заданий по расписанию"
  [settings]
  (println "-----------------------")
  (println "Starting schedules ")
  (chime/start-all-schedules! settings)
  (println "schedules started ..."))


(defn run-processing-cmd
  "Команда для начала процессинга загрузки данных с файла"
  [settings [processing-id]]
  (println "----------------------------------------")
  (println (str "Start processing: " processing-id))
  (let [db-uri           (:db-uri settings)
        file-proc-config (edn/read-string (slurp (:files-proc-edn-path settings)))
        proc-configs     (edn/read-string (slurp (:processing-configs-path settings)))
        result  (pm/run-local-file-processing db-uri
                                              file-proc-config
                                              proc-configs
                                              processing-id)]
    (println "processing results: ")
    (println result)
    (println "----------------------------------------")
    (println "processed successfully")))


(defn init-db-cmd
  "Команда для пересоздания БД"
  [settings]
  (println "----------------------------------------")
  (println "Start re-creating db...")
  (let [{:keys [db-uri schema-edn-path init-data-edn-path]} settings
        schema-txs (edn/read-string (slurp schema-edn-path))
        data-txs (-> init-data-edn-path
                     slurp
                     edn/read-string
                     eval)]
    (dbm/init-from-scratch db-uri schema-txs data-txs)
    (println "----------------------------------------")
    (println "db created successfully")))


(defn ensure-schema-cmd
  "Команда для установки схемы БД (idempotent) из schema.edn файла"
  [settings]
  (println "----------------------------------------")
  (println "Ensuring schema...")
  (let [{:keys [db-uri schema-edn-path]} settings
        conn       (dbm/new-conn db-uri)
        schema-txs (edn/read-string (slurp schema-edn-path))]
    (du/transact-and-return conn schema-txs)
    (println "----------------------------------------")
    (println "Schema ensured successfully")))


(defn transact-edn-cmd
  "Команда для загрузки tx данных в БД из edn файла"
  [settings [edn-file-name]]
  (println "----------------------------------------")
  (println "Transacting txs from file: " edn-file-name)
  (let [conn (dbm/new-conn (:db-uri settings))
        txs  (-> edn-file-name
                 slurp
                 edn/read-string
                 eval)]
    (du/transact-and-return conn txs)
    (println "----------------------------------------")
    (println "txs transacted successfully")))


;; -- Остальные функции -------------------------------------------------------
(def commands-map
  "Мэп со всеми доступными командами"
  {"init-db"           init-db-cmd
   "run-processing"    run-processing-cmd
   "ensure-schema"     ensure-schema-cmd
   "transact-edn"      transact-edn-cmd
   "start-schedules"   start-schedules-cmd
   "exit"              exit-cmd})


(defn read-command-line-loop
  "Запустить цикл чтения команды с консоли и выполнения"
  [settings]
  (while true
    (println "")
    (print "Enter your command > ")
    (flush)
    (let [input (read-line)
          cmds (-> input
                   (cljstr/split #" "))]
      (if-let [command (get commands-map (first cmds))]
        (try
          (let [c-args (->> cmds
                            rest
                            (map read-string))]
            (if-not (empty? c-args)
              (command settings c-args)
              (command settings)))
          (catch clojure.lang.ArityException e
            (println "Wrong arguments passed to command"))
          (catch Exception e
            (println "Error on executing command!")
            (pprint e)))
        (do
          (println "Command not found. Available commands: ")
          (println (->> commands-map
                        keys
                        (cljstr/join ", "))))))))
