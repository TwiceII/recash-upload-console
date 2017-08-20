(ns recash-upload-console.domain.datomic-utils
  "Функции и утилиты для работы с Datomic"
  (:require [datomic.api :as d]
            [com.stuartsierra.component :as component]
            [io.pedestal.log :as log]
            [clj-time.core :as t]
            [clojure.pprint :refer [pprint]]
            [recash-upload-console.common.security-utils :as su]
            [clj-time.coerce :as ct]))


;; -- Служебные функции -------------------------------------------------------
(defn transact-and-return
  "Выполнить транзакцию и вернуть результат"
  ([conn txs-coll]
   (transact-and-return conn txs-coll true))
  ([conn txs-coll wrap-in-vector?]
   (let [txs (cond-> txs-coll
                     wrap-in-vector?  (into []))] ;; переводим в векторную форму
     (log/debug :msg (str "transaction: " txs))
     (println "-- transacted txs:")
     (pprint txs)
     (deref (d/transact conn txs)))))


(defn new-uuid
  []
  (d/squuid))


(defn new-password
  [s]
  (su/md5 s))


(defn rollback-tx
  "Откатить транзакцию по номеру
   (возвращает все предыдущие значения datom-ов )
  WARNING: потенциально долгая ф-ция
  взято с:
  https://stackoverflow.com/questions/25389807/how-do-i-undo-a-transaction-in-datomic"
  [conn tx]
  (let [tx-log (-> conn d/log (d/tx-range tx nil) first) ; find the transaction
        txid   (-> tx-log :t d/t->tx) ; get the transaction entity id
        newdata (->> (:data tx-log)   ; get the datoms from the transaction
                     (remove #(= (:e %) txid)) ; remove transaction-metadata datoms
                     ; invert the datoms add/retract state.
                     (map #(do [(if (:added %) :db/retract :db/add) (:e %) (:a %) (:v %)]))
                     reverse)] ; reverse order of inverted datoms.
    @(d/transact conn newdata)))  ; commit new datoms.


;; -- Хэлперы функции ---------------------------------------------------------
(defn q-by-uuid [attrs]
  "Получить динамический datalog запрос"
  (apply conj '[:find] (list 'pull '?e attrs)
              '[:in $ ?name-k ?name-v
                :where [?e ?name-k ?name-v]]))

(defn get-uuid-by-name
  "Получить uuid сущности по какому-то полю имени"
  [uuid-k name-k name-v conn]
  (-> (d/q (q-by-uuid [uuid-k])
           (d/db conn) name-k name-v)
      ffirst
      uuid-k))


(defn q-pull-where
  "Получить datalog запрос с pull и where"
  [pull-pattern in-q where-q]
  (into []
    (concat (apply conj [[:find] (list 'pull '?e pull-pattern)])
            (apply conj [:in '$] in-q)
            (apply conj [:where] where-q))))


(defn q-pull-pattern
  [pattern where-attr]
  (q-pull-where pattern
                ['[?e where-attr]]))


(defn retract-entity-tx
  "Получить транзакцию для удаления сущности по uuid"
  [e-uuid-k e-uuid-v]
  [[:db/retractEntity [e-uuid-k e-uuid-v]]])

(defn retract-entities-txs
  "Получить транзакции на удаление (ретракт) по id сущностей"
  [eids]
  (for [eid eids]
    [:db/retractEntity eid]))


(defn value-of-attr-for-e
  "Значение аттрибута для entity-id e"
  [db e attr]
  (-> (d/datoms db :eavt e attr)
      first
      (.v)))

(defn field-by-other-field
  "Получить значение одного поля по значению второго"
  [attr-1 attr-2 v-2 conn]
  (->> (d/q {:find '[[?v1]]
             :in '[$ ?v2]
             :where [['?e attr-1 '?v1]
                     ['?e attr-2 '?v2]]}
            (d/db conn) v-2)
       first))


;; -- Компонет для системы ----------------------------------------------------

(defrecord DatomicComponent
  [datomic-config conn tx-report-queue]
  component/Lifecycle
  (start [component]
    (println "creating DatomicComponent")
    (println datomic-config)
    (let [url (:uri datomic-config)
          ; deleted? (d/delete-database url)
          created? (d/create-database url)
          conn (d/connect url)
          tx-report-queue (d/tx-report-queue conn)
          component (assoc component :conn conn :tx-report-queue tx-report-queue)]
      component))
  (stop [component]
    (d/release conn)
    (assoc component :conn nil)))

(defn new-datomic-component []
  (map->DatomicComponent {}))
