(ns powermail.parsers.datetime
  (:refer-clojure :exclude [time])
  (:use powermail.core
        [powermail.parsers common date time duration])
  (:require [clj-time.core :as time]
            [dj.peg :as peg])
  (:gen-class))


(def datetime
  (peg/|
    duration-spec
    (peg/alt (peg/s date-spec cws+ time-spec)
             (fn [[d _ t]] (concat d t)))
    (peg/alt (peg/s time-spec cws+ date-spec)
             (fn [[t _ d]] (concat d t)))
    (peg/alt date-spec
             (fn [d] (conj d (time/hour *time-ref*)
                             (time/minute *time-ref*)
                             (.getZone *time-ref*))))
    (peg/alt time-spec
             (fn [t] (flatten (conj [(time/year *time-ref*)
                                     (time/month *time-ref*)
                                     (time/day *time-ref*)] t))))))
