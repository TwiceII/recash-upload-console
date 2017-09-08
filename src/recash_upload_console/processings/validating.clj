(ns recash-upload-console.processings.validating)


(defmulti validate-mapped-item
  (fn [conn mapped-item mapped-item-type] mapped-item-type))
