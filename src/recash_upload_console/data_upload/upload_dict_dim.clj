(ns recash-upload-console.data-upload.upload-dict-dim
  (:require [recash-upload-console.domain.datomic-utils :as du]
            [recash-upload-console.domain.model :as m]
            [recash-upload-console.domain.db-manager :as dbm]
            [recash-upload-console.common.time-utils :as tu]
            [recash-upload-console.common.utils :as u]
            [recash-upload-console.data-upload.upload-specs :as upl-s]
            [recash-upload-console.data-upload.upload-service :as upl]
            [recash-upload-console.common.open-handlers :as open-handlers]
            [recash-upload-console.common.xml-utils :as xml-u]
            [clojure.spec :as s]
            [datomic.api :as d]
            [recash-upload-console.processings.etl :as etl]
            [recash-upload-console.processings.parsing-and-mapping
             :refer [item->custom->v
                     ignore-mapping?
                     item->field-value
                     item-has-field?
                     map-item-via
                     v-by-type] :as pnm]
            [recash-upload-console.processings.loading
             :refer [ent->load-item] :as l]
            [recash-upload-console.processings.validating
             :refer [validate-mapped-item]]
            [recash-upload-console.processings.processing :as prcs]))

;; -- Testing
(def db-uri
  "datomic:sql://recash-local?jdbc:postgresql://localhost:5432/recash-local?user=datomic&password=datomic")

(defn get-conn
  []
  (dbm/new-conn db-uri))


;; -- ET Side -----------------------------------------------------------------

(defmethod validate-mapped-item :dict-dim
  [conn mapped-item mapped-item-type]
  (if (s/valid? ::recash-upload-console.data-upload.upload-specs/dict-dim mapped-item)
   []
   [(s/explain-str ::recash-upload-console.data-upload.upload-specs/dict-dim mapped-item)]))


(defmethod ent->load-item [:dict-dim :datomic-tx]
  [conn mapped-item mapped-item-type load-item-type]
  (let [e mapped-item]
    (println "ent->load-item")
    (println e)
    (if (= :D (:dict-dim/op-type e))
      ;; при удалении
      (if-let [e-to-delete (m/e-of-foreign-dimension conn e)]
        [[:db/retractEntity e-to-delete]]
        ;; TODO: стоит ли выкидывать исключение или просто игнорить?
        (do
          (println "IGNORE because couldn't find dim to delete: " e)
          :ignore))
        ; (throw (Exception. "Не найдено измерение для удаления: " e)))
      ;; если новое или редактирование
      (if-let [dim-group-eid (when (:dict-dim/group-name e)
                               (m/e-of-dim-group-by-name conn (:dict-dim/group-name e)))]
        (-> (u/info-map m
              ;; транзакция по измерению
              :dim-tx (-> {:dimension/name (:dict-dim/name e)
                           :dimension/editable? (:dict-dim/editable? e)
                           :dimension/group (:db/id dim-group-eid)
                           :source/imported-datetime (tu/now-jdate)}
                          (merge (select-keys e [:source/name :source/frgn-uuid :source/frgn-str-id]))
                          (dissoc :op-type)
                          ;; если уже есть в базе - значит редактирование, иначе добавление
                          (#(if-let [exist-e (m/e-of-foreign-dimension conn e)]
                               (assoc % :db/id (:db/id exist-e))
                               (assoc % :dimension/uuid (d/squuid))))
                          (u/remove-nil-keys)))
            :dim-tx)
        (throw (Exception. (str "Не найдена группа измерений для добавления нового измерения"
                                e)))))))


(ent->load-item (get-conn)
                {:dict-dim/op-type :U
                 :dict-dim/group-name "Контрагенты"
                 :dict-dim/editable? false
                 :dict-dim/name "Новый клиент"
                 :source/name "ummastore"
                 :source/frgn-str-id "client5345"}
                :dict-dim
                :datomic-tx)
