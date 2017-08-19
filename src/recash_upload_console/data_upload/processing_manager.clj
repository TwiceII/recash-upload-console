(ns recash-upload-console.data-upload.processing-manager
  (:require [recash-upload-console.data-upload.upload-service :as u-s]
            [recash-upload-console.data-upload.upload-1c]
            [recash-upload-console.data-upload.upload-entries]
            [recash-upload-console.data-upload.upload-rule-table]
            [recash-upload-console.common.open-handlers :as open-handlers]
            [datomic.api :as d]))


(defn run-local-file-processing
  [db-uri file-proc-config upload-configs proc-id]
  (let [proc (get file-proc-config proc-id)
        proc-type (:processing-type proc)
        upl-config (get upload-configs (:processing-id proc))
        conn (d/connect db-uri)]
    (open-handlers/open-and-process
      (first proc-type)
      (second proc-type)
      (:file-name proc)
      (fn [d]
        (u-s/process-source-with-config conn
                                        upl-config
                                        d))
      {})))
