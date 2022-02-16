(ns powermail.parsers.time
  (:refer-clojure :exclude [time])
  (:use powermail.core
        powermail.parsers.common)
  (:require [dj.peg :as peg]
            [clj-time.core :as time])
  (:import org.joda.time.DateTimeZone)
  (:gen-class))


(def hour
  (peg/alt one-or-two-digit
           (fn [h]
             (if (> h 23) (throw (Exception. "Hour too large"))
               h))))
(def hour-minute
  (peg/alt (peg/s hour (literal ":") two-digit)
           (fn [[h _ m]]
             (if (> m 59)
               (throw (Exception. "Minute too large"))
               [h m]))))

(def timezone
  (peg/|
    (peg/alt (apply peg/|
                    (let [ids (DateTimeZone/getAvailableIDs)]
                      (map #(literal (.toLowerCase %1) %2) ids ids)))
             (fn [id] (DateTimeZone/forID id)))
    (peg/alt (peg/s (peg/t #"[+-]") hour-minute)
             (fn [[s [h m]]]
               (if (= s "-")
                 (DateTimeZone/forOffsetHoursMinutes (- 0 h) m)
                 (DateTimeZone/forOffsetHoursMinutes h m))))))

(def time-base
  (peg/|
    (peg/alt (peg/s hour-minute ws* (peg/t #"(am)|(pm)"))
             (fn [[hm _ m]]
               (cond (= 0 (first hm)) (throw (Exception. "Hour too small"))
                     (> (first hm) 12) (throw (Exception. "Hour too large"))
                     (> (last hm) 59) (throw (Exception. "Minute too large"))
                     (= m "am") (if (= (first hm) 12)
                                  [0 (last hm)]
                                  hm)
                     (= m "pm") (if (= (first hm) 12)
                                  [12 (last hm)]
                                  [(+ 12 (first hm)) (last hm)])
                     :else (throw (Exception. "Bad time")))))
    (peg/alt (peg/s hour ws* (peg/t #"(am)|(pm)"))
             (fn [[h _ m]]
               (cond (= m "am") (if (= h 12) [0 0] [h 0])
                     (= m "pm") (if (= h 12) [12 0] [(+ 12 h) 0]))))
    (peg/alt (peg/t #"(noon)|(midday)")
             (fn [_] [12 0]))
    (peg/alt (literal "midnight")
             (fn [_] [0 0]))
    hour-minute))

(def time
  (peg/|
    (peg/alt (peg/s time-base ws* timezone)
             (fn [[t _ tz]] (conj t tz)))
    (peg/alt time-base
             (fn [t] (conj t (.getZone *time-ref*))))))

(def time-spec
  (peg/|
    (peg/alt (peg/s (peg/t #"at\s+") time)
             (fn [[_ t]] t))
    (peg/alt (peg/s (literal "in") ws+ digits+ ws+ (peg/t #"minutes?"))
             (fn [[_ _ d _ _]]
               (let [time-ref (time/plus *time-ref* (time/minutes d))]
                 [(time/hour time-ref)
                  (time/minute time-ref)
                  (.getZone time-ref)])))
    (peg/alt (peg/s (literal "in") ws+ digits+ ws+ (peg/t #"hours?"))
             (fn [[_ _ d _ _]]
               (let [time-ref (time/plus *time-ref* (time/hours d))]
                 [(time/hour time-ref)
                  (time/minute time-ref)
                  (.getZone time-ref)])))
    time))