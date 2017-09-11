(ns recash-upload-console.data-upload.processing-manager
  (:require [recash-upload-console.data-upload.upload-service :as u-s]
            [recash-upload-console.data-upload.upload-1c]
            [recash-upload-console.data-upload.upload-entries]
            [recash-upload-console.data-upload.upload-rule-table]
            [recash-upload-console.common.open-handlers :as open-handlers]
            [recash-upload-console.processings.processing :as prcs]
            [recash-upload-console.processings.etl :as etl]
            [recash-upload-console.data-upload.upload-standard-entry]
            [datomic.api :as d]))


(defn run-local-file-processing
  [db-uri file-proc-config processing-configs proc-id]
  (let [proc (get file-proc-config proc-id)
        proc-type (:processing-type proc)
        proc-config (get processing-configs (:processing-id proc))
        conn (d/connect db-uri)]
    (println "proc-config: ")
    (println proc-config)
    (open-handlers/open-and-process
      (first proc-type)
      (second proc-type)
      (:file-name proc)
      (fn [d]
        (prcs/process-source-with-config conn d proc-config))
      {})))
