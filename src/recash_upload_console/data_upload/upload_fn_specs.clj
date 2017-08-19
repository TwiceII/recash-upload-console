(ns recash-upload-console.data-upload.upload-fn-specs
  (:require [clojure.spec :as s]
            [orchestra.spec.test :as st]
            [clojure.spec.test :as stest]
            [clojure.string :as cljstr]
            [recash-upload-console.common.common-specs :as sp-c]
            [recash-upload-console.domain.model-specs :as sp-m]
            [recash-upload-console.data-upload.upload-specs :as sp-u]
            [recash-upload-console.common.utils :as u]
            [recash-upload-console.data-upload.upload-service :as u-s]
            [recash-upload-console.data-upload.upload-entries :as u-e]
            [recash-upload-console.data-upload.upload-rule-table :as u-rt]
            [recash-upload-console.data-upload.upload-1c :as u-1c]))


;; -- Функции -----------------------------------------------------------------
(s/fdef u-s/process-source-with-config
  :args (s/cat :conn   ::sp-c/conn
               :config ::sp-u/config
               :source ::sp-u/source)
  :ret ::sp-u/process-source-result)


(s/fdef u-s/process-stage-with-logs
  :args (s/cat :conn ::sp-c/conn
               :stage-config ::sp-u/stage
               :source ::sp-u/source)
  :ret ::sp-u/stage-result)


(s/fdef u-s/process-stage
  :args (s/cat :conn ::sp-c/conn
               :stage-config ::sp-u/stage
               :source ::sp-u/source)
  :ret ::sp-u/process-result)


(s/fdef u-s/process-self
  :args (s/cat :conn ::sp-c/conn
               :stage-config ::sp-u/stage)
  :ret ::sp-u/process-result)


(s/fdef u-s/process-source
  :args (s/cat :conn ::sp-c/conn
               :stage-config ::sp-u/stage
               :source ::sp-u/source)
  :ret ::sp-u/process-result)


(s/fdef u-s/process-source-by-items-default
  :args (s/cat :conn ::sp-c/conn
               :stage-config ::sp-u/stage
               :source ::sp-u/source)
  :ret ::sp-u/process-result)


(s/fdef u-s/get-items-from-source
  :args (s/cat :stage-config ::sp-u/stage
               :source ::sp-u/source)
  :ret ::sp-u/source-item)


(s/fdef u-s/process-item-default
  :args (s/cat :conn ::sp-c/conn
               :stage-config ::sp-u/stage
               :item ::sp-u/source-item)
  :ret ::sp-u/process-result)


(s/fdef u-s/ent->tx-form
  :args (s/cat :conn ::sp-c/conn
               :ek ::sp-u/ent-k
               :e ::sp-u/unif-entity)
  :ret ::sp-u/ent-txs)


(s/fdef u-s/validate-ent
  :args (s/cat :conn ::sp-c/conn
               :ek ::sp-u/ent-k
               :e ::sp-u/unif-entity)
  :ret (s/nilable ::sp-u/validation-errors))


(s/fdef u-s/parse-item-default
  :args (s/cat :parse-params ::sp-u/parse-params
               :item ::sp-u/source-item)
  :ret ::sp-u/unif-entity)


(s/fdef u-s/many-fields-in-mappings
  :args (s/cat :mapping-params ::sp-u/mappings-map)
  :ret set?)


(s/fdef u-s/get-field-from-item
  :args (s/cat :parse-type :parse-params/parse-type
               :item ::sp-u/source-item
               :field-id some?)
  :ret (s/nilable some?))


(s/fdef u-s/transform-post
  :args (s/cat :ek ::sp-u/ent-k
               :transform-params map?
               :parsed-item some?)
  :ret ::sp-u/unif-entity)


(s/fdef u-s/transform-pre
  :args (s/cat :ek ::sp-u/ent-k
               :transform-params map?
               :item ::sp-u/source-item)
  :ret some?)

(s/fdef u-s/parse-by-type
  :args (s/cat :mapping-params ::sp-u/field-mapping
               :v some?)
  :ret some?)

(s/fdef u-s/parse-custom
  :args (s/cat :ek ::sp-u/ent-k
               :mapping-params ::sp-u/field-mapping
               :v some?)
  :ret some?)

(s/fdef u-1c/package-already-processed?
  :args (s/cat :conn ::sp-c/conn
               :package-number ::sp-u/package-number)
  :ret boolean?)

(s/fdef u-1c/write-package-to-history
  :args (s/cat :conn ::sp-c/conn
               :package-number ::sp-u/package-number
               :status ::sp-u/package-status)
  :ret some?)
