(ns recash-upload-console.domain.model
  "Доменная часть данных: ф-ции для записывания/считывания с БД"
  (:require [datomic.api :as d]
            [recash-upload-console.domain.datomic-utils :as du]))


(def dim-group-name->uuid
  (partial du/get-uuid-by-name :dim-group/uuid :dim-group/name))

(def rule-table-name->uuid
  (partial du/get-uuid-by-name :rule-table/uuid :rule-table/name))

(defn dimension-name->uuid
  [dim-name dim-group-name conn]
  (->> (d/q '[:find ?dim-uuid
              :in $ ?dim-name ?dg-name
              :where
                [?e :dimension/uuid ?dim-uuid]
                [?e :dimension/name ?dim-name]
                [?e :dimension/group ?dg]
                [?dg :dim-group/name ?dg-name]]
             (d/db conn) dim-name dim-group-name)
       ffirst))


(defn dimension-uuid-1c->uuid
  "Получить uuid измерения по его 1C uuid"
  [dim-uuid-1c conn]
  (->> (d/q '[:find ?dim-uuid
              :in $ ?dim-uuid-1c
              :where
                [?e :dimension/uuid ?dim-uuid]
                [?e :dimension/uuid-1c ?dim-uuid-1c]]
             (d/db conn) dim-uuid-1c)
       ffirst))
