(ns recash-upload-console.domain.model
  "Доменная часть данных: ф-ции для записывания/считывания с БД"
  (:require [datomic.api :as d]
            [recash-upload-console.domain.datomic-utils :as du]))

;; -- Testing
(def db-uri
  "datomic:sql://recash-local?jdbc:postgresql://localhost:5432/recash-local?user=datomic&password=datomic")

(defn get-conn
  []
  (d/connect db-uri))


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



(defn id-pair-for-foreign-entity
  [e]
  (if-let [uuid (:source/frgn-uuid e)] ; приоритет у uuid
    [:source/frgn-uuid uuid]
    [:source/frgn-str-id  (:source/frgn-str-id e)]))


(defn e-of-foreign-entity
  "Получить entity импортированной сущности"
  [e-uuid conn e]
  (let [[id-attr id-val] (id-pair-for-foreign-entity e)
        src-name (:source/name e)
        db (d/db conn)
        query {:find '[?e]
               :in '[$ ?id ?src-name]
               :where [['?e e-uuid]
                       ['?e id-attr '?id]
                       ['?e :source/name '?src-name]]}]
    (when id-val
      (->> (d/q query db id-val src-name)
           ffirst
           (#(do
               (println "d/entity-for: " %)
               (d/entity db %)))
           not-empty))))


(def e-of-foreign-entry (partial e-of-foreign-entity :entry/uuid))
(def e-of-foreign-dimension (partial e-of-foreign-entity :dimension/uuid))


(defn dim-group-by-name
  "Получить entity группы измерений по названию"
  [conn dg-name]
  (let [db (d/db conn)]
    (-> (d/q '[:find ?e
               :in $ ?dg-name
               :where [?e :dim-group/name ?dg-name]]
             db dg-name)
        ffirst
        (#(d/entity db %))
        not-empty)))
