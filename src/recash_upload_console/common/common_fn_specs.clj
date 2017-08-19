(ns recash-upload-console.common.common-fn-specs
  (:require [clojure.spec :as s]
            [orchestra.spec.test :as st]
            [clojure.spec.test :as stest]
            [clojure.string :as cljstr]
            [recash-upload-console.common.common-specs :as sp-c]
            [recash-upload-console.common.utils :as u]
            [recash-upload-console.common.open-handlers :as o-h]))


;; -- open handlers -----------------------------------------------------------
(s/fdef o-h/get-stream-to-open
  :args (s/cat :src-type  ::sp-c/src-type
               :open-type ::sp-c/open-type
               :src       ::sp-c/src
               :params    ::sp-c/params)
  :ret some?)


(s/fdef o-h/read-in-open
  :args (s/cat :src    ::sp-c/src
               :s-type ::sp-c/src-type
               :params ::sp-c/params)
  :ret some?)


(s/fdef o-h/open-and-process
  :args (s/cat :src-type   ::sp-c/src-type
               :open-type  ::sp-c/open-type
               :src        ::sp-c/src
               :process-fn fn?
               :params     ::sp-c/params)
  :ret (s/nilable some?))


(s/fdef o-h/do-with-open
  :args (s/cat :src-type   ::sp-c/src-type
               :open-type  ::sp-c/open-type
               :src        ::sp-c/src
               :process-fn fn?
               :params     ::sp-c/params)
  :ret (s/nilable some?))
