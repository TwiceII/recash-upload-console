(ns recash-upload-console.data-upload.upload-entries
  "Для загрузки записей"
  (:require [recash-upload-console.data-upload.upload-service
              :refer [parse-custom
                      validate-ent
                      transform-post
                      process-self
                      match-from-mapping
                      ent->tx-form]]
            [recash-upload-console.common.parsing-utils :as pu]
            [recash-upload-console.common.utils :as u]
            [recash-upload-console.common.time-utils :as tu]
            [recash-upload-console.domain.model :as m]
            [recash-upload-console.domain.datomic-utils :as du]
            [datomic.api :as d]
            [clojure.string :as cljstr]))


;; -- Парсинг -----------------------------------------------------------------

(defmethod parse-custom [:entry :dims]
  [ek mapping-params v]
  (let [group-name (get-in mapping-params [:params :group-name])
        dim-name (-> v (cljstr/trim))]
      {:dimension/name dim-name
       :dim-group/name group-name}))


;; -- Валидация ---------------------------------------------------------------
(defmethod validate-ent :entry
  [conn ek e]
  (cond-> []
          (nil? (:date e)) (conj "Не введена дата записи")))

;; -- Обработка групп измерений -----------------------------------------------
(defmethod process-self :dim-groups
  [conn stage-config]
  (u/info-map m
    :tx-form (->> (:process-data stage-config)
                  ;; TODO: добавить запись в БД
                  (map (fn [dg]
                         (-> dg
                             (assoc :dim-group/uuid (d/squuid))
                             (clojure.set/rename-keys
                               {:name        :dim-group/name
                                :editable?   :dim-group/editable?
                                :order-index :dim-group/order-index
                                :css-class   :dim-group/css-class})))))
    :tx-result (du/transact-and-return conn (:tx-form m))))


;; -- Унифиц.сущность записи -> Транзакционный вид -----------------------------------
(defn temp-dim-id
  "Временное строковое id для измерения"
  [dim-name dim-group]
  (str dim-name "-" dim-group))


(defn new-dim-tx
  "Транзакция для добавления нового измерения"
  [conn dim-name group-name]
  (-> {:dimension/uuid (d/squuid)
       :dimension/name dim-name
       :db/id (temp-dim-id dim-name group-name)
       :dimension/group [:dim-group/uuid (m/dim-group-name->uuid group-name conn)]}))


(defn dim->tx-map
  "Конвертировать унифиц.измерение в транзакционную форму"
  [conn dim]
  (let [{dim-name :dimension/name group-name :dim-group/name} dim]
    ;; есть ли уже такое измерение в базе
    (if-let [uuid (m/dimension-name->uuid dim-name group-name conn)]
      ;; уже есть
      {:type    :exists
       :tx      [:dimension/uuid uuid]}
      ;; еще нет, нужно добавить
      {:type    :new
       :pre-txs [(new-dim-tx conn dim-name group-name)]
       :tx      (temp-dim-id dim-name group-name)})))


(defmethod ent->tx-form :entry
  [conn ek e]
  (let [dims-txs (map #(dim->tx-map conn %) (:dims e))
        ;; транзакция для самой записи
        e-tx (-> e
                 (assoc :dims (->> dims-txs
                                   (map :tx)
                                   (into [])))
                 (clojure.set/rename-keys {:date      :entry/date
                                           :v-summ    :entry/summ
                                           :v-type    :entry/v-type
                                           :v-flow    :entry/v-flow
                                           :dims      :entry/dims
                                           :editable? :entry/editable?})
                 (assoc :entry/uuid (d/squuid)))
        ;; транзакции для предв. создания новых измерений
        pre-txs (into [] (mapcat :pre-txs dims-txs))]
    ;; объединяем транзакции
    (conj pre-txs e-tx)))
