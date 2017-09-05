(ns recash-upload-console.data-upload.upload-standard-entry
  (:require [recash-upload-console.domain.datomic-utils :as du]
            [recash-upload-console.domain.model :as m]
            [recash-upload-console.domain.db-manager :as dbm]
            [recash-upload-console.common.time-utils :as tu]
            [recash-upload-console.common.utils :as u]
            [datomic.api :as d]))


(defn id-pair-for-foreign-entity
  [e]
  (if-let [uuid (:source/frgn-uuid e)] ; приоритет у uuid
    [:source/frgn-uuid uuid]
    [:source/frgn-str-id  (:source/frgn-uuid e)]))


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
    (->> (d/q query db id-val src-name)
         first
         (d/entity db)
         not-empty)))



(def e-of-foreign-entry (partial e-of-foreign-entity :entry/uuid))
(def e-of-foreign-dimension (partial e-of-foreign-entity :dimension/uuid))


(defn temp-id-for-st-dim
  "Получить временный id для измерения"
  [dim]
  (str (:source/name dim)
       "_"
       (or (:source/frgn-uuid dim)
           (:source/frgn-str-id dim)
           (:st-dim/name dim))))


(defn new-st-dim-txs
  "Получить датомы для добавления нового стандартного измерения"
  [conn dim]
  (let [group-name (:st-dim/group-name dim)]
    (if-let [dg-eid (:db/id (du/attr-val->e-single conn :dim-group/name group-name))]
      (-> {:dimension/uuid (d/squuid)
           :dimension/name (:st-dim/name dim)
           :dimension/editable? (:st-dim/editable? dim)
           :db/id (temp-id-for-st-dim dim)
           :dimension/group dg-eid
           :source/imported-datetime (tu/now-jdate)}
          (merge (select-keys dim [:source/name :source/frgn-uuid :source/frgn-str-id])))
      (throw (Exception. (str "Не найдена группа с названием: " group-name))))))


(defn standard-dim->tx-form
  "Получить транзакционную форму для стандартного измерения"
  [conn dim]
  (let [{dim-type        :st-dim/type
         src-frgn-uuid   :source/frgn-uuid
         src-frgn-str-id :source/frgn-str-id
         src-name        :source/name
         dim-name        :st-dim/name
         dg-name         :st-dim/group-name} dim]
    (-> (u/info-map m
          ;; entity уже существующего измерения
          :exist-e (or (e-of-foreign-dimension conn dim)     ; по foreign id
                       (du/attr-val->e-single conn           ; по названию
                                              :dimension/name
                                              (:st-dim/name dim)))
          ;; tx для уже существующего измерения
          :exist-tx-form (when (:exist-e m)
                          {:type :exists
                           :dim-e (:db/id (:exist-e m))})
          ;; конечная форма tx
          :result-tx-form
            (if (:exist-tx-form m)
              (:exist-tx-form m) ; если уже есть, просто возвращаем
              ;; иначе проверяем тип измерения
              (case dim-type
                ;; если обязательно должно уже существовать
                :must-pre-exist
                   ;; выкидываем исключение
                   (throw (Exception. (str "Не найдено обязательное измерение для uuid:"
                                           (:source/frgn-uuid dim) ", str-id: "
                                           (:source/frgn-str-id dim) ", name: "
                                           (:st-dim/name dim))))
                ;; если можно добавлять новое измерение
                :addable
                  {:type    :new
                   :pre-txs [(new-st-dim-txs conn dim)]
                   :dim-e   (temp-id-for-st-dim dim)}
                ;; если тип измерения не найден
                (throw (Exception. (str "Type " dim-type " of dim not found"))))))
        :result-tx-form)))


(defn standard-entry->tx-form
  "Получить транзакцию из стандартной записи"
  [conn e]
  (if (= :D (:st-entry/op-type e))
    ;; при удалении
    [[:db/retractEntity (e-of-foreign-entity conn e)]]
    ;; если новое или редактирование
    (-> (u/info-map m
          ;; транзакции по измерениям
          :dims-tx-forms (map #(standard-dim->tx-form conn %) (:st-entry/dims e))
          ;; транзакция по самой записи
          :e-tx (-> {:entry/date      (:st-entry/date e)
                     :entry/summ      (:st-entry/summ e)
                     :entry/v-flow    (:st-entry/v-flow e)
                     :entry/v-type    (:st-entry/v-type e)
                     :entry/editable? (:st-entry/editable? e)
                     :entry/dims      (->> (:dims-tx-forms m)
                                           (map :dim-e)
                                           (into []))}
                    (merge (select-keys e [:source/name :source/frgn-uuid :source/frgn-str-id]))
                    (dissoc :op-type)
                    ;; если уже есть в базе - значит редактирование, иначе добавление
                    (#(if-let [exist-e (e-of-foreign-entry conn e)]
                         (assoc % :db/id exist-e)
                         (assoc % :entry/uuid (d/squuid))))
                    (u/remove-nil-keys))
          :pre-txs (into [] (mapcat :pre-txs (:dims-tx-forms m)))
          ;; объединяем транзакции
          :result-txs (conj (:pre-txs m) (:e-tx m)))
        :result-txs)))



; (standard-dim->tx-form
;   (get-conn)
;   {:st-dim/type        :addable
;    :st-dim/name        "Без договора565"
;    :st-dim/group-name  "Договоры"
;    :st-dim/editable?   false
;    :source/name        "1C"
;    :source/frgn-uuid   #uuid "568d03ea-be27-4e34-a86f-7ae703a66be4"})

; (standard-entry->tx-form
;   (get-conn)
;   {:st-entry/uuid      #uuid "498d03ea-be27-4e34-a86f-7ae703a66bef"
;    :st-entry/op-type   :U ; #{:U :D}
;    :st-entry/date      #inst "2017-04-06T00:00:00"
;    :st-entry/summ      45453.65
;    :st-entry/v-flow    :inflow
;    :st-entry/v-type    :fact
;    :st-entry/editable? false
;    :source/name        "ummastore"
;    :source/frgn-str-id "sales-55"
;    :source/frgn-uuid   #uuid "1c8d03ea-be27-4e34-a86f-7ae703a66bef"
;    :st-entry/dims      [{:st-dim/type        :addable
;                          :st-dim/name        "name cntr"
;                          :st-dim/group-name  "Контрагенты"
;                          :st-dim/editable?   false
;                          :source/name        "1C"
;                          :source/frgn-uuid   #uuid "568d03ea-be27-4e34-a86f-7ae703a66be5"}
;                         {:st-dim/type        :addable
;                          :st-dim/name        "Без договора"
;                          :st-dim/group-name  "Договоры"
;                          :st-dim/editable?   false
;                          :source/name        "1C"
;                          :source/frgn-uuid   #uuid "568d03ea-be27-4e34-a86f-7ae703a66be4"}]})


; {:st-dim/uuid        #uuid "498d03ea-be27-4e34-a86f-7ae703a66be4"
;  :st-dim/type        :must-pre-exist ; #{:must-pre-exist :addable}
;  :st-dim/name        "Договор-234234"
;  :st-dim/group-name  "Договоры"
;  :st-dim/editable?   false
;  :source/name        "1C"
;  :source/frgn-uuid   #uuid "568d03ea-be27-4e34-a86f-7ae703a66be4"
;  :source/frgn-str-id "dogovor-44234"}
;
; {:st-entry/uuid      #uuid "498d03ea-be27-4e34-a86f-7ae703a66bef"
;  :st-entry/op-type   :U ; #{:U :D}
;  :st-entry/date      #inst "2017-04-06T00:00:00"
;  :st-entry/summ      45453.65
;  :st-entry/v-flow    :inflow ; #{:inflow :outflow}
;  :st-entry/v-type    :fact ; #{:plan :fact}
;  :st-entry/editable? false
;  :source/name        "ummastore"
;  :source/frgn-str-id "sales-55"
;  :source/frgn-uuid   #uuid "1c8d03ea-be27-4e34-a86f-7ae703a66bef"
;  :st-entry/dims      []} ; см. st-dim

; (comment
;   {:st-entry/date #inst "2017-04-06T00:00:00"
;    :st-entry/frgn-str-id "ummastore-sales-34"
;    :st-entry/frgn-uuid #uuid "3435-sdfsd-5345-dgdf"
;    :st-entry/frgn-source "ummastore"
;    :st-entry/op-type :U ; #{:U :D}
;    :st-entry/summ 435435.65
;    :st-entry/v-flow :inflow
;    :st-entry/v-type :fact
;    :st-entry/editable? false
;    :st-entry/dims [{:st-dim/type :pre-exist
;                     :st-dim/name "Новый контрагент"
;                     :st-dim/ext-str-id "client-34234"
;                     :st-dim/ext-uuid #uuid "324a-54546-7567-876c"
;                     :st-dim/group-name "Контрагенты"}
;                    {:st-dim/name "Старый договор с новым названием"
;                     :st-dim/ext-str-id "dogovor-44234"
;                     :st-dim/ext-uuid #uuid "3245-ca546-7567-876c"
;                     :st-dim/group-name "Договоры"}]}
;
;   ;; если только по существующим id
;   {:st-dim/type :pre-exist
;    :st-dim/frgn-str-id "client-342342"
;    :st-dim/frgn-source
;    :st-dim/group-name "Клиенты"}
;
;   ;; если по uuid и можно добавлять новые
;   {:st-dim/type :addable
;    :st-dim/frgn-uuid #uuid "45345-4534-g56df-sdsf"
;    :st-dim/name "Договор №5454-2017"
;    :st-dim/group-name "Договоры"}
;
;   ;; если только по названиям и можно добавлять новые
;   {:st-dim/type :addable
;    :st-dim/name "Новый контрагент"
;    :st-dim/group-name "Контрагент"})



; (defn standard-entry->tx-form
;   [conn e]
;   (if (= :D (:st-entry/op-type e))
;     ;; при удалении
;     [[:db/retractEntity (id-of-st-entry e)]]
;     ;; если новое или редактирование
;     (let [dims-txs (map #(standard-dim->tx-form conn %) (:dims e))
;           e-tx (-> e
;                    (assoc :dims (->> dims-txs
;                                      (map :tx)
;                                      (into [])))
;                    (dissoc :op-type)
;                    ;; добавляем префикс entry
;                    (u/update-keys #(->> % (name) (str "entry/") keyword))
;                    (assoc :entry/upload-date-1c (tu/now-jdate))
;                    ;; если уже есть с таким uuid-1c - значит редактирование, иначе добавление
;                    (#(if-let [exist-uuid (du/field-by-other-field :entry/uuid
;                                                                   :entry/uuid-1c
;                                                                   (:entry/uuid-1c %)
;                                                                   conn)]
;                         (assoc % :entry/uuid exist-uuid)
;                         (assoc % :entry/uuid (d/squuid))))
;                    (u/remove-nil-keys))
;           ;; транзакции для предв. создания новых измерений
;           pre-txs (into [] (mapcat :pre-txs dims-txs))]
;         ;; объединяем транзакции
;         (conj pre-txs e-tx))))
