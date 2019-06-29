(ns powermail.parsers.duration
  (:refer-clojure :exclude [time])
  (:use powermail.core
        [powermail.parsers common date time])
  (:require [dj.peg :as peg]
            [clj-time.core :as time])
  (:gen-class))


(def duration-span
  (peg/t #"(\s*-\s*)|(\s+to\s+)|(\s+until\s+)"))

(def time-duration
  (peg/|
    (peg/alt (peg/s time-spec duration-span time)
             (fn [[s _ e]] s))
    (peg/alt (peg/s one-or-two-digit duration-span time)
             (fn [[h _ t]]
               (if (and (> (first t) 12)
                        (< h 12)
                        (< (+ h 12) (first t)))
                 (cons (+ 12 h) (rest t))
                 (cons h (rest t)))))
    (peg/alt (peg/s time-spec duration-span one-or-two-digit)
             (fn [[t _ _]] t))))

(def date-duration
  (peg/|
    (peg/alt (peg/s one-or-two-digit duration-span date)
             (fn [[n _ d]]
               (conj (take 2 d) n)))
    (peg/alt (peg/s date-spec duration-span date-spec)
             (fn [[d _ _]] d))))

(def date-time-duration
  (peg/|
    (peg/alt (peg/s date-spec cws+ (literal "from") ws+ time-duration)
             (fn [[d _ _ _ t]] (concat d t)))
    (peg/alt (peg/s date-spec cws+ time-duration)
             (fn [[d _ t]] (concat d t)))
    (peg/alt (peg/s time-duration cws+ (literal "on") ws+ date-spec)
             (fn [[t _ _ _ d]] (concat d t)))
    (peg/alt (peg/s time-duration cws+ date-spec)
             (fn [[t _ d]] (concat d t)))))

(def duration
  (peg/|
    date-time-duration
    (peg/alt date-duration
             (fn [d] (concat d [(time/hour *time-ref*)
                                (time/minute *time-ref*)
                                (.getZone *time-ref*)])))
    (peg/alt time-duration
             (fn [t] (concat [(time/year *time-ref*)
                              (time/month *time-ref*)
                              (time/day *time-ref*)] t)))))

(def duration-spec
  (peg/|
   (peg/alt (peg/s (literal "from") ws+ duration)
            (fn [[_ _ d]] d))
   duration))
