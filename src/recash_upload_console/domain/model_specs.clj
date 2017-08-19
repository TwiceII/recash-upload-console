(ns recash-upload-console.domain.model-specs
  (:require [clojure.spec :as s]
            [orchestra.spec.test :as st]
            [clojure.spec.test :as stest]
            [clojure.string :as cljstr]
            [recash-upload-console.domain.datomic-utils :as du]
            [recash-upload-console.common.utils :as u]
            [recash-upload-console.common.time-utils :as tu]
            [recash-upload-console.domain.model :as m]))


(s/def ::not-empty #(not (empty? %)))
(defn sorted-asc-insts?
  [v]
  (= v (sort tu/jdates-before? v)))

;; общие
(s/def ::conn some?) ;; сделать проверку типа потом
(s/def ::db some?)
(s/def ::e some?)

(s/def ::id uuid?)
(s/def ::n (s/and int? #(and (> % 0) (< % 1000))))
(s/def ::name (s/and string? (comp not cljstr/blank?)))
(s/def ::direction #{:before :today :after})
(s/def ::search-dim-str (s/nilable string?))
(s/def ::date inst?)
(s/def ::nilable-date (s/nilable ::date))
(s/def ::sorted-dates (s/and (s/coll-of ::date :distinct true)
                             sorted-asc-insts?))
(s/def ::active-dim-group-ids set?)
(s/def ::grouping-mode #{:by-day :by-month :by-year})
(s/def ::date-params (s/keys :req-un [::grouping-mode]))
(s/def ::uuid uuid?)
;; положение даты отн-но диапазона дат
(s/def ::date-pos-to-range #{:in-range :before-range :after-range})

;; для всех мэпов, где ключ = id внутри значения
(s/def ::map-w-ids
  (s/and (fn [m] (every? #(= % (get-in m [% :id])) (keys m)))
         ::not-empty))

;; тапл
(s/def ::tuple
  (s/and (s/map-of ::uuid ::uuid)
         ::not-empty))

;; тапл для поиска
(s/def ::search-tuple
  (s/nilable (s/map-of ::uuid (s/coll-of ::uuid))))

(s/def ::ruled-dims (s/coll-of ::uuid :distinct true))
(s/def ::fact double?)
(s/def ::plan double?)

;; параметры для поиска
(s/def ::search-params (s/keys :req-un [::n ::direction ::active-dim-group-ids ::date-params]
                               :opt-un [::search-dim-str ::nilable-date]))

;; плоская запись с datomic
(s/def ::v-flow #{:inflow :outflow})
(s/def ::v-type #{:fact :plan})
(s/def ::v-summ double?)
(s/def ::dims ::tuple)
(s/def ::editable? boolean?)
(s/def ::plain-entry
  (s/keys :req-un [::id ::date ::v-type ::v-flow ::v-summ ::dims]
          :opt-un [::editable?]))

;; униф. запись
(s/def ::fpsum
  (s/and
    (s/keys :opt [::fact ::plan])
    #(not (empty? %))))
(s/def ::date-values (s/map-of ::date ::fpsum))
(s/def ::before-sum (s/nilable ::fpsum))
(s/def ::after-sum  (s/nilable ::fpsum))
(s/def ::unif-entry (s/keys :req-un [::tuple]
                            :opt-un [::before-sum ::after-sum ::ruled-dims ::date-values]))
(s/def ::others-flow-entry (s/keys :opt-un [::before-sum ::after-sum]))

;; мэп с таплом и суммой (для сумм до и после)
(s/def ::tuple-sum-map
  (s/map-of ::tuple ::fpsum))

;; flow-entries (униф.записи по потокам)
(s/def ::flow-entries (s/map-of ::v-flow (s/coll-of ::unif-entry)))

;; измерения и группы измерений
(s/def ::order-index int?)
(s/def ::css-class string?)
(s/def ::group-id ::uuid)
(s/def ::dimension
  (s/keys :req-un [::id ::name ::group-id]))
;; карта с измерениями
(s/def ::dims-map
  (s/and ::map-w-ids
         (s/map-of ::uuid ::dimension)))
;; группа измерения
(s/def :dim-group/dims ::dims-map)
(s/def ::dim-group
  (s/keys :req-un [::id ::name ::editable? ::order-index :dim-group/dims]
          :opt-un [::css-class]))
;; карта с группами измерений
(s/def ::dim-groups-map
  (s/and ::map-w-ids
         (s/map-of ::uuid ::dim-group)))
;; короткая версия группы измерений (только имя и id)
(s/def ::short-dim-group
  (s/keys :req-un [::id ::name]))

;; таблица правил и правила
;; правило
(s/def :rule/from ::dims-map)
(s/def :rule/to ::dimension)
(s/def ::rule
  (s/keys :req-un [::id ::from ::to]))
;; Таблица правил
(s/def ::group-to ::short-dim-group)
(s/def ::groups-from (s/coll-of ::short-dim-group :distinct true))
(s/def ::rules
  (s/or ::empty #(u/nil-or-empty? %)
        ::some-rules (s/and ::map-w-ids
                            (s/map-of ::uuid ::rule))))
(s/def ::rule-table
  (s/keys :req-un [::id ::group-to ::groups-from]
          :opt-un [::rules]))
(s/def ::rule-tables-map
  (s/and ::map-w-ids
         (s/map-of ::uuid ::rule-table)))


;; entries-search-info
(s/def ::has-after? boolean?)
(s/def ::has-before? boolean?)
(s/def ::before-date (s/nilable ::date))
(s/def ::after-date  (s/nilable ::date))
(s/def ::inflow-others
  (s/or :empty   empty?
        :ba-sums (s/keys :req-un [::before-sum ::after-sum])))
(s/def ::outflow-others
  (s/or :empty   empty?
        :ba-sums (s/keys :req-un [::before-sum ::after-sum])))
(s/def ::entries-search-info
  (s/and (s/keys :req-un [::flow-entries
                          ::before-date
                          ::after-date
                          ::has-before?
                          ::has-after?
                          ::inflow-others
                          ::outflow-others])
         #(or (nil? (:before-date %))
              (nil? (:after-date %))
              (not (tu/jdates-before? (:after-date %)
                                      (:before-date %))))))


; (s/valid? ::flow-entries
;   {:inflow [],
;    :outflow [{:after-sum {:plan 104250.0},
;               :before-sum {:fact 125325.51000000001},
;               :date-values
;                 {#inst "2017-03-15T00:00:00.000-00:00" {:fact 92296.96},
;                  #inst "2017-04-18T00:00:00.000-00:00" {:fact 131521.96},
;                  #inst "2017-05-01T00:00:00.000-00:00" {:plan 34750.0},
;                  #inst "2017-06-01T00:00:00.000-00:00" {:plan 34750.0},
;                  #inst "2017-07-01T00:00:00.000-00:00" {:plan 34750.0},
;                  #inst "2017-08-01T00:00:00.000-00:00" {:plan 34750.0},
;                  #inst "2017-09-01T00:00:00.000-00:00" {:plan 34750.0},
;                  #inst "2017-03-16T00:00:00.000-00:00" {:fact 5255.74},
;                  #inst "2017-10-01T00:00:00.000-00:00" {:plan 34750.0},}
;               :tuple {#uuid "592bdaba-0f04-4fd6-9e06-3b26c3cfbb68" #uuid "592bdad2-a22c-4c1f-9f1e-46800dfa38eb"}}
;              {:before-sum {:fact 8427950.89},
;               :tuple {#uuid "592bdaba-0f04-4fd6-9e06-3b26c3cfbb68" #uuid "592bdad2-d4ab-4c9f-b4b8-70ad88f17280"}}]})
