(ns recash-upload-console.data-upload.upload-specs
  (:require [clojure.spec :as s]
            [orchestra.spec.test :as st]
            [clojure.spec.test :as stest]
            [clojure.string :as cljstr]
            [recash-upload-console.common.common-specs :as sp-c]
            [recash-upload-console.domain.model-specs :as sp-m]
            [recash-upload-console.common.utils :as u]
            [recash-upload-console.common.time-utils :as tu]))


(s/def ::source some?)
(s/def ::source-item some?)
(s/def ::nat ::sp-c/natural)
(s/def ::ent-k keyword?)
(s/def ::unif-entity some?)

(s/def ::ent-txs
  (s/or :tx-form     ::sp-c/tx-form
        :ignore-form #{:ignore}))


;; -- spec для upload-config --------------------------------------------------
(s/def :field-mapping/display-name string?)
(s/def :field-mapping/field keyword?)
(s/def :field-mapping/type keyword?)
(s/def :field-mapping/params map?)
(s/def ::field-mapping (s/keys :req-un [:field-mapping/display-name
                                        :field-mapping/type
                                        :field-mapping/field]
                               :opt-un [:field-mapping/params]))

(s/def ::mappings-map (s/map-of some? ::field-mapping))


(s/def :parse-params/parse-type keyword?)
(s/def :parse-params/parse-to keyword?)
(s/def :parse-params/mappings ::mappings-map)
(s/def :parse-params/transform-pre (s/keys :req-un [::ent-k]))
(s/def :parse-params/transform-post (s/keys :req-un [::ent-k]))
(s/def ::parse-params
  (s/keys :req-un [:parse-params/parse-type :parse-params/parse-to
                   :parse-params/mappings]
          :opt-un [:parse-params/transform-pre :parse-params/transform-post]))



(s/def :stage/stage-id keyword?)
(s/def :stage/stage-type #{:process-self :process-source})
(s/def :stage/run-when (s/or :consts #{:always :no-errors}
                             :prev-stages (s/coll-of keyword? :kind vector?)))
(s/def :stage/process-ent keyword?)
(s/def :stage/process-data some?)
(s/def :stage/source-processing #{:by-item :batch})
(s/def :stage/parse-params ::parse-params)
(s/def ::stage
  (s/keys :req-un [:stage/stage-id :stage/stage-type
                   :stage/run-when]
          :opt-un [:stage/process-ent :stage/process-data
                   :stage/source-processing :stage/parse-params]))


(s/def :processing/id keyword?)
(s/def :processing/name string?)
(s/def :processing/stages (s/coll-of ::stage))
(s/def ::processing (s/keys :req-un [:processing/id :processing/name :processing/stages]))


(s/def ::processings-map
  (s/and ::recash-upload-console.common.common-specs/map-w-ids
         (s/map-of :processing/id ::processing)))


;; -- остальные spec ----------------------------------------------------------

(s/def ::config ::processing)

(s/def :stats/total ::nat)
(s/def :stats/successes ::nat)
(s/def :stats/failures ::nat)
(s/def :stats/did-not-run ::nat)
(s/def ::stats
  (s/and (s/keys :req-un [:stats/total :stats/successes :stats/failures :stats/did-not-run])
         #(= (:total %)
             (+ (:successes %) (:failures %) (:did-not-run %)))))


(s/def :stage-result/result #{:success :failure})
(s/def :stage-result/data some?)
(s/def :stage-result/exception some?)
(s/def ::stage-result
  (s/and (s/keys :req-un [:stage-result/result]
                 :opt-un [:stage-result/data :stage-result/exception])
         #(case (:result %)
            :success (= (into #{} (keys %)) #{:result :data})
            :failure (= (into #{} (keys %)) #{:result :exception}))))


(s/def ::stage-id keyword?)
(s/def ::stage-results-map (s/map-of ::stage-id ::stage-result))

(s/def :process-source-result/stage-results ::stage-results-map)
(s/def :process-source-result/stats ::stats)
(s/def ::process-source-result
  (s/and (s/keys :req-un [:process-source-result/stage-results
                          :process-source-result/stats])
         #(= (get-in % [:stats :total])
             (count (keys (:stage-results %))))))

(s/def ::process-result some?)

(s/def ::validation-errors vector?)


;; ============================================================================
;; upload 1C
(s/def ::package-number string?)
(s/def ::package-status #{:success :failure})

(s/def :entry-1c/uuid-1c uuid?)
(s/def :entry-1c/op-type #{:D :U})
(s/def :entry-1c/v-flow ::sp-m/v-flow)
(s/def :entry-1c/v-type ::sp-m/v-type)
(s/def :entry-1c/summ ::sp-m/v-summ)
(s/def :entry-1c/date ::sp-m/date)
(s/def :entry-1c/editable? boolean?)
(s/def :entry-1c/currency-rate-1c double?)
(s/def :entry-1c/currency-1c string?)
(s/def :entry-1c/currency-summ-1c double?)
(s/def :entry-1c/number-1c (s/nilable string?))
(s/def :entry-1c/doc-type-1c (s/nilable string?))
(s/def :entry-1c/comment-1c (s/nilable string?))
(s/def :entry-1c/in-number-1c (s/nilable string?))
(s/def :entry-1c/in-date-str-1c (s/nilable string?))
(s/def :entry-1c/booking-account-1c (s/nilable string?))
(s/def :entry-1c/purpose-1c (s/nilable string?))

(s/def :dim-1c/name ::recash-upload-console.common.common-specs/not-whitespace-string)
(s/def :dim-1c/group-name ::recash-upload-console.common.common-specs/not-whitespace-string)
(s/def :dim-1c/uuid-1c uuid?)
(s/def :entry-1c/dim-1c (s/keys :req-un [:dim-1c/name :dim-1c/group-name
                                         :dim-1c/uuid-1c]))
(s/def :entry-1c/dims (s/coll-of :entry-1c/dim-1c :kind vector?))

(s/def ::entry-1c-spec
  (s/or :update
        (s/and #(= (:op-type %) :U)
                (s/keys :req-un [:entry-1c/v-type :entry-1c/v-flow :entry-1c/summ
                                 :entry-1c/date :entry-1c/dims
                                 :entry-1c/op-type :entry-1c/uuid-1c :entry-1c/editable?
                                 :entry-1c/currency-rate-1c :entry-1c/currency-1c :entry-1c/currency-summ-1c]
                        :opt-un [:entry-1c/doc-type-1c :entry-1c/comment-1c :entry-1c/in-number-1c
                                 :entry-1c/in-date-str-1c :entry-1c/purpose-1c :entry-1c/booking-account-1c
                                 :entry-1c/number-1c]))
        :delete
        (s/and #(= (:op-type %) :D)
               (s/keys :req-un [::op-type :entry-1c/uuid-1c]))))

;; -- Для source полей
(s/def :source/name string?)
(s/def :source/frgn-uuid uuid?)
(s/def :source/frgn-str-id string?)
(s/def :source/imported-datetime inst?)
(s/def :source/required (s/or :w-uuid (s/keys :req [:source/name :source/frgn-uuid]
                                              :opt [:source/imported-datetime])
                              :w-str-id (s/keys :req [:source/name :source/frgn-str-id]
                                                :opt [:source/imported-datetime])))
(s/def :source/optional (s/keys :req [:source/name]
                                :opt [:source/imported-datetime]))
(s/def :source/common (s/or :none :source/optional
                            :required :source/required))


;; -- Для стандартных entry и dimension ---------------------------------------
;; standard dim для entry
(s/def :st-dim/type #{:addable :must-pre-exist})
(s/def :st-dim/name string?)
(s/def :st-dim/group-name string?)
(s/def :st-dim/editable? boolean?)

(s/def ::st-dim
  (s/or :pre-exist (s/and #(= :must-pre-exist (:st-dim/type %))
                          (s/merge :source/common
                                   (s/keys :req [:st-dim/type :st-dim/group-name :st-dim/editable?])))
        :addable   (s/and #(= :addable (:st-dim/type %))
                          (s/merge :source/common
                                   (s/keys :req [:st-dim/type :st-dim/name :st-dim/group-name :st-dim/editable?])))))

; (s/def :st-dim/common (s/keys :req [:st-dim/type :st-dim/name :st-dim/group-name :st-dim/editable?]))
;
; (s/def ::st-dim (s/merge :st-dim/common :source/common))

;; standard entry
(s/def :st-entry/op-type #{:D :U})
(s/def :st-entry/date inst?)
(s/def :st-entry/uuid uuid?)
(s/def :st-entry/summ double?)
(s/def :st-entry/v-flow #{:inflow :outflow})
(s/def :st-entry/v-type #{:plan :fact})
(s/def :st-entry/editable? boolean?)
(s/def :st-entry/dims (s/coll-of ::st-dim))

(s/def :st-entry/common
  (s/or :to-ignore #(= :ignore %)
        :to-delete (s/and #(= :D (:st-entry/op-type %))
                          (s/keys :req [:st-entry/op-type]))
        :to-upsert (s/and #(= :U (:st-entry/op-type %))
                          (s/keys :req [:st-entry/date
                                        :st-entry/op-type
                                        :st-entry/summ
                                        :st-entry/v-flow
                                        :st-entry/v-type
                                        :st-entry/editable?
                                        :st-entry/dims]
                                  :opt [:st-entry/uuid]))))

(s/def ::st-entry (s/merge :st-entry/common :source/common))


;; -- Измерения в виде справочников
;; (обычно используются внутри standard-entry как must-pre-exist)
;; синхронизируются с источниками

(s/def :dict-dim/uuid uuid?)
(s/def :dict-dim/name string?)
(s/def :dict-dim/group-name string?)
(s/def :dict-dim/op-type #{:D :U})
(s/def :dict-dim/editable? boolean?)
(s/def :dict-dim/common
  (s/or :to-ignore #(= :ignore %)
        :to-delete (s/and #(= :D (:dict-dim/op-type %))
                          (s/keys :req [:dict-dim/op-type]))
        :to-upsert (s/and #(= :U (:dict-dim/op-type %))
                          (s/keys :req [:dict-dim/name
                                        :dict-dim/group-name]
                                  :opt [:dict-dim/uuid]))))
(s/def ::dict-dim (s/merge :dict-dim/common :source/common))


; (s/explain ::dict-dim
;   {:dict-dim/name "Магазин Алматы"
;    :dict-dim/group-name "Магазины"
;    :dict-dim/op-type :U
;    :source/name "ummastore"
;    :source/frgn-str-id "store-24"})
;
; (s/explain ::dict-dim
;   {:dict-dim/op-type :D
;    :source/name "ummastore"
;    :source/frgn-str-id "store-45"})




; (s/explain :st-entry/common
;   {:source/frgn-uuid #uuid "568d03ea-be27-4e34-a86f-7ae703a66be4",
;    :st-entry/editable? false,
;    :st-entry/summ nil,
;    :st-entry/v-type :fact,
;    :st-entry/v-flow nil,
;    :source/name "1С",
;    :st-entry/date nil,
;    :st-entry/dims [],
;    :st-entry/op-type :D})
;
; (s/explain :st-entry/common
;   :ignore)
;
;
; (s/explain :st-entry/common
;   {:source/frgn-uuid #uuid "568d03ea-be27-4e34-a86f-7ae703a66be4",
;    :source/name "1C"
;    :st-entry/op-type :D})
