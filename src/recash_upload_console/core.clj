(ns recash-upload-console.core
  (:require [recash-upload-console.commands :as cmds]
            [clojure.edn :as edn])
  (:gen-class))


(defn get-project-config
  "Получить настройки проекта"
  []
  (edn/read-string (slurp "config/project-config.edn")))


;; -- main --------------------------------------------------------------------

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Starting uploader ...")
  (let [settings (get-project-config)]
    (cmds/read-command-line-loop settings)))
