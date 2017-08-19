(ns recash-upload-console.domain.db-manager
  (:require [datomic.api :as d]
            [datofu.all]
            [clojure.pprint :refer [pprint]]
            [recash-upload-console.domain.datomic-utils :as du]))



(defn init-from-scratch
  "Загрузить начальные данные: схемы и 3 группы измерения"
  [db-uri schema-txs init-data-txs]
  ;; удаляем и создаем бд
  (d/delete-database db-uri)
  (d/create-database db-uri)
  ;; новое подключение
  (let [conn (d/connect db-uri)]
    ;; datofu функции и схемы
    (du/transact-and-return conn (datofu.all/schema-tx))
    ;; загружаем все схемы
    (du/transact-and-return conn schema-txs)
    ;; загружаем данные по умолчанию
    (du/transact-and-return conn init-data-txs)
    ;; возвращаем новое подключение
    conn))
