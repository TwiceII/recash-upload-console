(ns recash-upload-console.common.common-specs
  (:require [clojure.spec :as s]
            [orchestra.spec.test :as st]
            [clojure.spec.test :as stest]
            [clojure.string :as cljstr]
            [recash-upload-console.common.utils :as u]))


(s/def ::conn some?)
(s/def ::natural (s/and int? #(not (neg? %))))
(s/def ::not-empty #(not (empty? %)))
(s/def ::not-whitespace-string (s/and string? #(not (cljstr/blank? %))))

;; для всех мэпов, где ключ = id внутри значения
(s/def ::map-w-ids
  (s/and (fn [m] (every? #(= % (get-in m [% :id])) (keys m)))
         ::not-empty))

(s/def ::tx-form some?)

;; -- open handlers -----------------------------------------------------------
(s/def ::src-type keyword?)
(s/def ::open-type keyword?)
(s/def ::src some?)
(s/def ::params map?)
