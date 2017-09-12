(ns recash-upload-console.processings.loading
  (:require [recash-upload-console.domain.datomic-utils :as du]
            [recash-upload-console.domain.model :as m]
            [recash-upload-console.domain.db-manager :as dbm]
            [recash-upload-console.common.time-utils :as tu]
            [recash-upload-console.common.utils :as u]
            [recash-upload-console.processings.parsing-and-mapping :as pnm]
            [datomic.api :as d]))


(defmulti ent->load-item
  (fn [conn mapped-item mapped-item-type load-item-type]
    [mapped-item-type load-item-type]))


(defn mapped-item->load-item
  "Получить load item для загрузки (обычно в БД) из mapped-item"
  [conn mapped-item  mapped-item-type  load-item-type]
  (if (= :ignore mapped-item)
    :ignore ; если нужно проигнорировать
    (ent->load-item conn mapped-item mapped-item-type load-item-type)))


(defmulti process-load-item
  "Обработка load-item для загрузки в БД"
  (fn [conn load-item load-item-type debug-mode?] load-item-type))


;; для datomic транзакций
(defmethod process-load-item :datomic-tx
  [conn load-item load-item-type debug-mode?]
  (if debug-mode?
    (do
      (println "DEBUG TRANSACTING DATOMIC TX: ")
      (println load-item))
    (du/transact-and-return conn load-item)))
