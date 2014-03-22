(ns powermail.parsers.duration-test
  (:require [clojure.test :refer :all]
            [powermail.core :refer :all]
            [powermail.parsers.duration :refer :all]
            [clj-time.core :as time]
            [dj.peg :as peg]))


(defn consume-all
  [parser input]
  (let [result (peg/parse parser input)]
    (if (= (result :unconsumed-input) "")
      (result :result) result)))

(defn consume-duration
  [input] (consume-all duration-spec input))

(def tz (.getZone *time-ref*))

(deftest test-duration
  (testing "duration parser"
    (is (= (consume-duration "13/4/2013 10-12pm") [2013 13 4 10 0 tz]))
    (is (= (consume-duration "10-12pm 13/4/2013") [2013 13 4 10 0 tz]))
    (is (= (consume-duration "13/4/2013 10 until 12pm") [2013 13 4 10 0 tz]))
    (is (= (consume-duration "13/4/2013 from 10-12pm") [2013 13 4 10 0 tz]))
    (is (= (consume-duration "3rd jan 2014 until tomorrow")
           [2014 1 3 (time/hour *time-ref*) (time/minute *time-ref*) tz]))))
