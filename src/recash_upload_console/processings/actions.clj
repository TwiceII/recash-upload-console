(ns recash-upload-console.processings.actions
  "Разнообразные действия над источником")


(defmulti run-action-on-source
  (fn [conn source action-params]
    (:action-type action-params)))
