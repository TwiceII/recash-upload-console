(ns recash-upload-console.data-upload.upload-rule-table
  "Для загрузки таблицы соответствий"
  (:require [recash-upload-console.data-upload.upload-service
              :refer [parse-custom
                      validate-ent
                      process-self
                      transform-post
                      ent->tx-form]
              :as upl-s]
            [recash-upload-console.data-upload.upload-entries :as upl-e]
            [recash-upload-console.common.parsing-utils :as pu]
            [recash-upload-console.common.utils :as u]
            [recash-upload-console.common.time-utils :as tu]
            [recash-upload-console.domain.model :as m]
            [recash-upload-console.domain.datomic-utils :as du]
            [datomic.api :as d]
            [clojure.string :as cljstr]))

(defmethod process-self :rule-table
  [conn stage-config]
  (u/info-map m
    :tx-form  (-> (:process-data stage-config)
                  (assoc :rule-table/uuid (d/squuid)
                         :rule-table/deletable? false)
                  (update :rule-table/groups-from
                    (fn [gs-from]
                      (mapv #(-> [:dim-group/uuid (m/dim-group-name->uuid % conn)]) gs-from)))
                  (update :rule-table/group-to
                    (fn [g-to]
                      [:dim-group/uuid (m/dim-group-name->uuid g-to conn)]))
                  (#(conj [] %)))
    :tx-result (du/transact-and-return conn (:tx-form m))))


(defmethod parse-custom [:rule :dims-from]
  [ek mapping-params v]
  (let [dim-name        (-> v (cljstr/trim))
        group-name      (get-in mapping-params [:params :group-name])]
    {:dimension/name dim-name
     :dim-group/name group-name}))


(defmethod parse-custom [:rule :dim-to]
  [ek mapping-params v]
  (let [dim-name        (-> v (cljstr/trim))
        group-name      (get-in mapping-params [:params :group-name])]
    {:dimension/name dim-name
     :dim-group/name group-name}))


(defmethod transform-post :rules-for-table
  [ek transform-params parsed-item]
  (assoc parsed-item :rule-table/name (:rule-table/name transform-params)))


(defmethod validate-ent :rule
  [conn ek e]
  (-> []))


(defmethod ent->tx-form :rule
  [conn ek r]
  (let [dims-from       (map #(upl-e/dim->tx-map conn %)
                              (:dims-from r))
        dim-to          (upl-e/dim->tx-map conn (:dim-to r))
        rule-table-uuid (m/rule-table-name->uuid (:rule-table/name r) conn)
        pre-txs         (into [] (mapcat :pre-txs (conj dims-from dim-to)))]
    (if rule-table-uuid
      (-> r
          (assoc :rule/from (->> dims-from (map :tx) (into [])))
          (assoc :rule/to (->> dim-to :tx))
          (assoc :rule/uuid (d/squuid))
          (assoc :rule-table/_rules [:rule-table/uuid rule-table-uuid])
          (dissoc :dims-from :dim-to :rule-table/name)
          (#(conj pre-txs %)))
      (throw (Exception. (str "Таблица правил "
                              (:rule-table/name r)
                              " не найдена (забыли создать?)"))))))


; (parse-item {:parse-type  :csv
;              :parse-to    :rule
;              :mappings    {0 {:display-name "Контрагент"
;                               :field        :dims-from
;                               :params       {:group-name "Контрагенты"}}
;                            1 {:display-name "Договор"
;                               :field        :dims-from
;                               :params       {:group-name "Договоры"}}
;                            2 {:display-name "Статья"
;                               :field        :dim-to
;                               :params       {:group-name "Статьи"}}}
;              ;; нужно доп-но применить ф-цию после мэппингов
;              :transform-post {:ent-k :rules-for-table
;                               :params {:rule-table/name "Статьи через контрагента и договор"}}}
;             ["200467187 (тенговый)" "" "Оплата услуг банка"])
;
; (def conn (d/connect "datomic:sql://recash-finish?jdbc:postgresql://localhost:5432/recash-finish?user=datomic&password=datomic"))
;
; (upl-s/process-stage-with-logs
;   conn
;   {:stage-id          :add-rules
;    :stage-type        :process-source
;    :run-when          [:precreate-rule-table]
;    :file-name         "importcsv/rules.csv"
;    :source-processing :by-item
;    :parse-params
;      {:parse-type  :csv
;       :parse-to    :rule
;       :mappings    {0 {:display-name "Контрагент"
;                        :field        :dims-from
;                        :params       {:group-name "Контрагенты"}}
;                     1 {:display-name "Договор"
;                        :field        :dims-from
;                        :params       {:group-name "Договоры"}}
;                     2 {:display-name "Статья"
;                        :field        :dim-to
;                        :params       {:group-name "Статьи"}}}
;       ;; нужно доп-но применить ф-цию после мэппингов
;       :transform-post {:ent-k :rules-for-table
;                        :params {:rule-table/name "Статьи через контрагента и договор"}}}})
