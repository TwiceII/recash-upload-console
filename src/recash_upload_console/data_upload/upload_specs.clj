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
