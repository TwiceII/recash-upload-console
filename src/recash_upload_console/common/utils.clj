(ns recash-upload-console.common.utils
  "Утилиты и вспомогательные функции"
  (:require [clojure.walk :as clj-walk]
            [clojure.string :as cs]))


(defn nil-or-empty? [x]
  (or (nil? x)
      (and (coll? x)
           (empty? x))))


(defn nil-or-empty-or-blank?
  [x]
  (or (nil-or-empty? x)
      (and (string? x)
           (cs/blank? x))))


(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))


(defn k-of-v
  "Получить ключ в хм у к-го значение равно нужному"
  [m v]
  (->> m
       vec
       (some #(when (= (second %) v)
                (first %)))))


(defn k-where-v
  "Получить ключ в хм у к-го значение удовлетворяет предикату"
  [m pred]
  (->> m
       vec
       (some #(when (pred (second %))
                (first %)))))



(defn replace-all-keys
  "Заменить все ключи в объекте на другие"
  [rm x]
  (clj-walk/postwalk
    (fn [el]
      (if (and (keyword? el)
               (contains? rm el))
        (get rm el)
        el))
    x))


(defn update-keys
  "Обновить ключи через ф-цию"
  [m update-fn]
  (reduce-kv (fn [nm k v]
               (assoc nm (update-fn k) v))
             {} m))


(defn remove-keys
  "Удалить ключи из хм"
  [m ks]
  (select-keys m (->> (keys m)
                      (filter #(not (in? ks %))))))


(defn map-index-by
  "Сгруппировать в хм по какому-то полю
  замена для group-id, когда предполагается,
  что одному ключу одно значение"
  [k seq-of-maps]
  (reduce (fn [res-m iter-m]
            (if-let [v (get iter-m k)]
              (assoc res-m v iter-m)
              res-m))
          {} seq-of-maps))


(defn find-some
  "Найти в векторе/списке элемент,
  который удовлетв. условию"
  [pred-fn l]
  (some #(when (pred-fn %) %) l))


(defn coll-contains-subcoll?
  "Все ли элементы подсписка находятся в списке?"
  [coll subcoll]
  (println "coll-contains-subcoll")
  (println coll)
  (println subcoll)
  (println "----------")
  (every? #(in? coll %) subcoll))


(defn take-n-before
  "Взять n элементов до элемента с перед. индексом (включ. этот элемент)"
  [all-v to-ind n]
  (->> all-v
       (take (inc to-ind))
       (take-last n)
       (into [])))


(defn take-n-after
  "Взять n элементов после элемента с перед.индексом (включ. этот элемент)"
  [all-v from-ind n]
  (->> all-v
       (drop from-ind)
       (take n)
       (into [])))

;; -- Threading macros extensions ---------------------------------------------
(defmacro assoc-info
  [prev mvar k & body]
  `((fn [m#]
      (let [~mvar m#]
        (assoc m# ~k
          (do ~@body)))) ~prev))


; (-> {}
;     (assoc-info m :info1 (+ 3 56))
;     (assoc-info m :info2
;       (let [p (:info1 m)]
;         (+ p 2000))))


(defmacro info-map
  "Создать информационный мэп"
  [mvar & forms]
  (let [assoc-tuples (->> forms
                          (partition 2)
                          (map #(-> (list `assoc-info 'm (first %) (second %)))))
        thr (concat `(-> {}) assoc-tuples)]
    thr))


; (info-map m
;   :info1 (+ 3 5)
;   :info2 (let [z (:info1 m)]
;           (+ z 6)))
; ==
; (-> {}
;     ((fn [m] (assoc m :info1 (+3 5))))
;     ((fn [m] (assoc m :info2 (let [z (:info1 m)]
;                                (+ z 6))))))


(defn subvector-nth-range
  "Получить подвектор с индекса до какого-то индекса"
  [v from-nth to-nth]
  (subvec v from-nth (inc to-nth)))


(defn indexed-hashmap-of-coll
  "Проиндексировать элементы в векторе и получить хм вида {элемент индекс_элемента}
  (предполагается что элементы НЕ повторяются)"
  [coll]
  (->> coll
       (keep-indexed #(-> [%2 %1]))
       (into {})))


(defn has-substr?
  "Есть ли в строке подстрока (независимо от регистров)"
  [s ss]
  (boolean (re-find (re-pattern (cs/lower-case ss)) (cs/lower-case s))))


(defn op-if-some
  "Применить какую-то операцию к значениям
   внутри fp1 fp2 по ключу key"
  [op key fp1 fp2]
  (when (or (some? (get fp1 key))
            (some? (get fp2 key)))
      (op (or (get fp1 key) 0)
          (or (get fp2 key) 0))))


(defn unlazy
  "Преобразовать коллекцию из lazy в не-lazy
  (для xml считывания с зипперов)"
  [coll]
  (let [unlazy-item (fn [item]
                      (cond
                        (sequential? item) (vec item)
                        (map? item) (into {} item)
                        :else item))
        result    (clojure.walk/postwalk unlazy-item coll)]
    result))


(defn unqualify-keyword
  "Убрать часть до / с keyword"
  [kw]
  (if (qualified-keyword? kw)
    (keyword (name kw))
    kw))


(defn unqualify-all-keys
  "Заменить все keyword ключи в объекте на unqualified"
  [x]
  (clj-walk/postwalk
    (fn [el]
      (if (keyword? el)
        (unqualify-keyword el)
        el))
    x))

(defn remove-nil-keys
  [m]
  (reduce-kv (fn [nm k v]
               (if (some? v)
                 (assoc nm k v)
                 nm))
             {} m))
