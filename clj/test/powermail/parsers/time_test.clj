(ns powermail.parsers.time-test
  (:require [clojure.test :refer :all]
            [powermail.core :refer :all]
            [powermail.parsers.time :refer :all]
            [clj-time.core :as time]
            [dj.peg :as peg])
  (:import org.joda.time.DateTimeZone))


(defn consume-all
  [parser input]
  (let [result (peg/parse parser input)]
    (if (= (result :unconsumed-input) "")
      (result :result) result)))

(defn consume-time
  [input] (consume-all time-spec input))

(def tz (.getZone *time-ref*))
(def tz-hobart (DateTimeZone/forID "Australia/Hobart"))
(def tz-prague (DateTimeZone/forID "Europe/Prague"))
(def tz-plus2 (DateTimeZone/forOffsetHours 2))
(def tz-minus0230 (DateTimeZone/forOffsetHoursMinutes -2 30))

(deftest test-time
  (testing "time parser"
    (is (= (consume-time "10:00") [10 0 tz]))
    (is (= (consume-time "00:00") [0 0 tz]))
    (is (= (consume-time "13:59") [13 59 tz]))
    (is (= (consume-time "2:50") [2 50 tz]))
    (is (= (consume-time "0:10") [0 10 tz]))
    (is (= (consume-time "2:50am") [2 50 tz]))
    (is (= (consume-time "2:50pm") [14 50 tz]))
    (is (= (consume-time "12:45pm") [12 45 tz]))
    (is (= (consume-time "1:45 pm") [13 45 tz]))
    (is (= (consume-time "12am") [0 0 tz]))
    (is (= (consume-time "12pm") [12 0 tz]))
    (is (= (consume-time "1pm") [13 0 tz]))
    (is (= (consume-time "noon") [12 0 tz]))
    (is (= (consume-time "midday") [12 0 tz]))
    (is (= (consume-time "midnight") [0 0 tz]))
    (is (thrown-with-msg? Exception #"Hour too large" (consume-time "25:20")))
    (is (thrown-with-msg? Exception #"Hour too large" (consume-time "13:01pm")))
    (is (thrown-with-msg? Exception #"Hour too large" (consume-time "13:01am")))
    (is (thrown-with-msg? Exception #"Minute too large" (consume-time "2:60")))
    (is (thrown-with-msg? Exception #"Minute too large" (consume-time "2:60am")))
    (is (thrown-with-msg? Exception #"Minute too large" (consume-time "2:60pm")))
    (is (thrown-with-msg? Exception #"Hour too small" (consume-time "0:23am")))
    (is (thrown-with-msg? Exception #"Hour too small" (consume-time "0:30pm")))))

(deftest test-timezone
  (testing "timezone parser"
    (is (= (consume-time "10:00 australia/hobart") [10 0 tz-hobart]))
    (is (= (consume-time "13:00 europe/prague") [13 0 tz-prague]))
    (is (= (consume-time "12:31+02:00") [12 31 tz-plus2]))
    (is (= (consume-time "12:31 -2:30") [12 31 tz-minus0230]))))
