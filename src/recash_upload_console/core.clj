(ns recash-upload-console.core
  (:require [clojure.string :as cljstr]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [datomic.api :as d]
            [datofu.all])
  (:gen-class))


(defn get-project-config
  "Получить настройки проекта"
  []
  (edn/read-string (slurp "config/project-config.edn")))

(defn squuid
  []
  (d/squuid))

(defn md5 [s]
  (let [algorithm (java.security.MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (java.math.BigInteger. 1 raw))))


(defn transact-and-return
  "Выполнить транзакцию и вернуть результат"
  [conn txs-coll]
  (let [txs (into [] txs-coll)] ;; переводим в векторную форму
    (println "txs: ")
    (pprint txs)
    (deref (d/transact conn txs))))


(defn transact-init-schemas
  [conn schema-edn-path]
  (let [schema-txs (edn/read-string (slurp schema-edn-path))]
    (println "schema: ")
    (pprint schema-txs)
    (transact-and-return conn schema-txs)))


(defn transact-init-data
  [conn init-data-edn-path]
  (println "start init data")
  (let [data-txs (-> init-data-edn-path
                     slurp
                     edn/read-string
                     eval)]
   (println "init data: ")
   (pprint data-txs)
   (transact-and-return conn data-txs)))


(defn init-db-cmd
  [settings]
  (println "----------------------------------------")
  (println "Start re-creating db...")
  (let [{:keys [db-uri schema-edn-path init-data-edn-path]} settings]
    ;; удаляем и создаем бд
    (d/delete-database db-uri)
    (d/create-database db-uri)
    ;; новое подключение
    (let [conn (d/connect db-uri)]
      ;; datofu функции и схемы
      (transact-and-return conn (datofu.all/schema-tx))
      ;; загружаем все схемы
      (transact-init-schemas conn schema-edn-path)
      ;; загружаем данные по умолчанию
      (transact-init-data conn init-data-edn-path)
      ;; возвращаем новое подключение
      (println "----------------------------------------")
      (println "db created successfully"))))



(defn exit-cmd
  [_]
  (println "Exiting...")
  (System/exit 0))






;; -- main --------------------------------------------------------------------

(def commands-map
   {"init-db" init-db-cmd
    "exit"    exit-cmd})


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [settings (get-project-config)]
    (println (:db-uri settings))
    (println "Starting uploader ...")
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
              (throw e)))
          (println "Command not found. Options: func1, func2, exit"))))))
