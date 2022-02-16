(ns powermail.parsers.date
  (:refer-clojure :exclude [time])
  (:use powermail.core
        powermail.parsers.common)
  (:require [dj.peg :as peg]
            [clj-time.core :as time])
  (:import java.text.DateFormatSymbols)
  (:gen-class))

(defn advance-year [d m]
  (loop [time-ref (time/date-time (time/year *time-ref*) m d)]
    (if (time/before? time-ref *time-ref*)
        (recur (time/plus time-ref (time/years 1)))
        [(time/year time-ref) m d])))

(def day-of-week
  ; DateFormatSymbols has 'Sunday'=0
  ; whereas joda time has Monday=1 and Sunday=7
  (let [rotate #(concat (rest %) [(first %)])
        symbols (DateFormatSymbols/getInstance)]
    (apply peg/|
      (concat
        (map-indexed #(literal %2 (+ 1 %1))
                     (map #(.toLowerCase %)
                       (rotate (rest (.getWeekdays symbols)))))
        (map-indexed #(literal %2 (+ 1 %1))
                     (map #(.toLowerCase %)
                       (rotate (rest (.getShortWeekdays symbols)))))))))

(def month
  (let [symbols (DateFormatSymbols/getInstance)]
    (apply peg/|
      (concat
       (map-indexed #(literal %2 (+ 1 %1))
                    (map #(.toLowerCase %)
                      (take 12 (.getMonths symbols))))
       (map-indexed #(literal %2 (+ 1 %1))
                    (map #(.toLowerCase %)
                      (take 12 (.getShortMonths symbols))))))))

(def year
  (peg/|
   four-digit
   (peg/alt two-digit #(+ 2000 %))))

(def day
  (peg/alt (peg/s one-or-two-digit (peg/t #"((st)|(nd)|(rd)|(th))?")) first))

(def date
  (peg/|
    (peg/alt (peg/s day-of-week cws+ month cws+ day cws+ year)
             (fn [[_ _ m _ d _ y]] [y m d]))
    (peg/alt (peg/s month cws+ day cws+ year)
             (fn [[m _ d _ y]] [y m d]))
    (peg/alt (peg/s day-of-week cws+ month cws+ day)
             (fn [[_ _ m _ d]] [(time/year *time-ref*) m d]))
    (peg/alt (peg/s month ws+ day)
             (fn [[m _ d]] [(time/year *time-ref*) m d]))
    (peg/alt (peg/s month ws+ four-digit)
             (fn [[m _ y]] [y m 1]))
    (peg/alt (peg/s day-of-week cws+ day cws+ month cws+ year)
             (fn [[_ _ d _ m _ y]] [y m d]))
    (peg/alt (peg/s day ws+ month cws+ year)
             (fn [[d _ m _ y]] [y m d]))
    (peg/alt (peg/s day-of-week cws+ day cws+ month)
             (fn [[_ _ d _ m]] [(time/year *time-ref*) m d]))
    (peg/alt (peg/s day ws+ month)
             (fn [[d _ m]] [(time/year *time-ref*) m d]))
    (peg/alt (peg/s
               one-or-two-digit (peg/t #"[\.\-\/]")
               one-or-two-digit (peg/t #"[\.\-\/]")
               two-or-four-digit)
             (fn [[a _ b _ y]]
               (cond (= *spec-ref* 'MDY) [y a b]
                     (= *spec-ref* 'DMY) [y b a])))
    (peg/alt (peg/s
               one-or-two-digit (peg/t #"[\.\-\/]") one-or-two-digit)
             (fn [[a _ b]]
               (cond (= *spec-ref* 'MDY) (advance-year a b)
                     (= *spec-ref* 'DMY) (advance-year b a))))))

(defn advance-to [day]
  (loop [time-ref *time-ref*]
    (if (= (time/day-of-week time-ref) day)
      [(time/year time-ref)
       (time/month time-ref)
       (time/day time-ref)]
      (recur (time/plus time-ref (time/days 1))))))

(defn advance-days [days]
  (let [time-ref (time/plus *time-ref* (time/days days))]
    [(time/year time-ref)
     (time/month time-ref)
     (time/day time-ref)]))

(def date-spec
  (peg/|
    date
    (peg/alt (literal "today")
             (fn [_] (advance-days 0)))
    (peg/alt (literal "tomorrow")
             (fn [_] (advance-days 1)))
    (peg/alt (peg/s (literal "on") ws+ date)
             (fn [[_ _ d]] d))
    (peg/alt (peg/s (literal "in") ws+ month)
             (fn [[_ _ m]] [(time/year *time-ref*) m 1]))
    (peg/alt (peg/s (peg/t #"(on)|(this)|(next)") ws+ day-of-week)
             (fn [[_ _ d]] (advance-to d)))
    (peg/alt (peg/s (literal "in") ws+ digits+ ws+ (peg/t #"days?"))
             (fn [[_ _ d _ _]] (advance-days d)))
    (peg/alt (peg/s (literal "in") ws+ digits+ ws+ (peg/t #"weeks?"))
             (fn [[_ _ d _ _]] (advance-days (* 7 d))))
    (peg/alt day-of-week
             (fn [d] (advance-to d)))))
