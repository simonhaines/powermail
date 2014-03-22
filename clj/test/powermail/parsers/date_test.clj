(ns powermail.parsers.date-test
  (:require [clojure.test :refer :all]
            [powermail.core :refer :all]
            [powermail.parsers.date :refer :all]
            [clj-time.core :as time]
            [dj.peg :as peg]))

(defn consume-all
  [parser time-ref input]
  (binding [*time-ref* (apply time/date-time time-ref)]
    (let [result (peg/parse parser input)]
      (if (= (result :unconsumed-input) "")
        (result :result) result))))

(defn consume-date [time-ref input]
  (consume-all date time-ref input))

(defn consume-spec [time-ref input]
  (consume-all date-spec time-ref input))

(deftest test-date
  (testing "date parser"
    (is (= (consume-date [2013 2 24] "feb 24") [2013 2 24]))
    (is (= (consume-date [2013 2 24] "sunday, feb 24, 2013") [2013 2 24]))
    (is (= (consume-date [2013 2 24] "saturday, feb 24, 2013") [2013 2 24]))
    (is (= (consume-date [2013 2 24] "february 24, 2013") [2013 2 24]))
    (is (= (consume-date [2013 2 24] "feb      24, 2013") [2013 2 24]))
    (is (= (consume-date [2013 2 24] "saturday, jan 24") [2013 1 24]))
    (is (= (consume-date [2013 2 24] "march 23rd") [2013 3 23]))
    (is (= (consume-date [2013 2 24] "sunday, 24th february, 2013") [2013 2 24]))
    (is (= (consume-date [2013 2 24] "saturday 24 feb 2013") [2013 2 24]))
    (is (= (consume-date [2013 2 24] "24 february 2013") [2013 2 24]))
    (is (= (consume-date [2013 2 24] "saturday 24 feb") [2013 2 24]))
    (is (= (consume-date [2013 2 24] "24th february") [2013 2 24]))
    (is (= (consume-date [2013 2 24] "23rd february") [2013 2 23]))
    (is (= (consume-date [2013 2 24] "25/12") [2013 12 25]))
    (is (= (consume-date [2013 12 26] "25/12") [2014 12 25]))
    (is (= (consume-date [2013 2 24] "feb 2014") [2014 2 1]))
    (is (= (binding [*spec-ref* 'MDY]
             (consume-date [2013 2 24] "2/24/2013")) [2013 2 24]))
    (is (= (binding [*spec-ref* 'MDY]
             (consume-date [2013 2 24] "2-24-2013")) [2013 2 24]))
    (is (= (binding [*spec-ref* 'MDY]
             (consume-date [2013 2 24] "2.24.2013")) [2013 2 24]))
    (is (= (binding [*spec-ref* 'DMY]
             (consume-date [2013 2 24] "24/2/2013")) [2013 2 24]))
    (is (= (binding [*spec-ref* 'DMY]
             (consume-date [2013 2 24] "24-2-2013")) [2013 2 24]))
    (is (= (binding [*spec-ref* 'DMY]
             (consume-date [2013 2 24] "24.2.2013")) [2013 2 24]))))

(deftest test-date-spec
  (testing "date-spec parser"
    (is (= (consume-spec [2013 2 24] "on feb 24") [2013 2 24]))
    (is (= (consume-spec [2013 2 24] "on 15/12") [2013 12 15]))
    (is (= (consume-spec [2013 2 24] "on 15/2") [2014 2 15]))
    (is (= (consume-spec [2013 2 24] "today") [2013 2 24]))
    (is (= (consume-spec [2013 2 28] "tomorrow") [2013 3 1]))
    (is (= (consume-spec [2012 2 28] "tomorrow") [2012 2 29]))
    (is (= (consume-spec [2013 10 18] "on fri") [2013 10 18]))
    (is (= (consume-spec [2013 10 18] "this sat") [2013 10 19]))
    (is (= (consume-spec [2013 10 18] "this monday") [2013 10 21]))
    (is (= (consume-spec [2013 10 18] "next monday") [2013 10 21]))
    (is (= (consume-spec [2013 10 18] "in march") [2013 3 1]))
    (is (= (consume-spec [2013 10 18] "in jan") [2013 1 1]))
    (is (= (consume-spec [2013 10 18] "in 1 day") [2013 10 19]))
    (is (= (consume-spec [2013 10 18] "in 5 days") [2013 10 23]))
    (is (= (consume-spec [2013 10 18] "in 1 week") [2013 10 25]))
    (is (= (consume-spec [2013 10 18] "in 5 weeks") [2013 11 22]))
    (is (= (consume-spec [2013 10 18] "thursday") [2013 10 24]))))
