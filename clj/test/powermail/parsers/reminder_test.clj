(ns powermail.parsers.reminder-test
  (:require [clojure.test :refer :all]
            [powermail.core :refer :all]
            [powermail.parsers.reminder :refer :all]
            [clj-time.core :as time]
            [dj.peg :as peg])
  (:import org.joda.time.DateTimeZone))


(defn consume-all
  [parser input]
  (let [result (peg/parse parser input)]
    (if (= (result :unconsumed-input) "")
      (result :result) result)))

(defn consume-reminder
  [[y m d h mn tz] input]
  (binding [*time-ref* (time/from-time-zone (time/date-time y m d h mn) tz)]
    (consume-all reminder input)))

(def tz (.getZone *time-ref*))

(deftest test-reminder
  (testing "reminder parser"
    (is (= (consume-reminder [2013 2 15 14 28 tz] "remind me on 15/12 i need to take out my trash #todo #garbage")
           {:who [:self]
            :when [2013 12 15 14 28 tz]
            :what "you need to take out your trash"
            :tags #{"todo" "garbage"}}))
    (is (= (consume-reminder [2013 2 15 14 28 tz] "remind me in 20 minutes to take out the garbage #todo #garbage")
           {:who [:self]
            :when [2013 2 15 14 48 tz]
            :what "take out the garbage"
            :tags #{"todo" "garbage"}}))
    (is (= (consume-reminder [2013 2 15 14 28 tz] "remind me tomorrow to get mel to take out my garbage for me")
           {:who [:self]
            :when [2013 2 16 14 28 tz]
            :what "get mel to take out your garbage for you"
            :tags #{}}))
    (is (= (consume-reminder [2013 11 3 17 52 tz] "remind me thursday when i need reminding #reminder")
           {:who [:self]
            :when [2013 11 7 17 52 tz]
            :what "when you need reminding"
            :tags #{"reminder"}}))
    (is (= (consume-reminder [2013 11 3 11 52 tz] "remind me at 5pm my cat's bowels need massaging #ick #kill-the-cat")
           {:who [:self]
            :when [2013 11 3 17 00 tz]
            :what "your cat's bowels need massaging"
            :tags #{"ick" "kill-the-cat"}}))
    (is (= (consume-reminder [2013 11 3 17 52 tz] "remind me tomorrow that i think 'the end of days' is in december")
           {:who [:self]
            :when [2013 11 4 17 52 tz]
            :what "you think 'the end of days' is in december"
            :tags #{}}))
    (is (= (consume-reminder [2013 11 3 16 52 tz] "remind me at 5pm myopia degrades intelligence")
           {:who [:self]
            :when [2013 11 3 17 00 tz]
            :what "myopia degrades intelligence"
            :tags #{}}))
    (is (= (consume-reminder [2013 11 3 16 52 tz] "remind me to ensure correct parsing in 10 minutes instead of what we have now")
           {:who [:self]
            :when [2013 11 3 17 02 tz]
            :what "ensure correct parsing instead of what we have now"
            :tags #{}}))
    (is (= (consume-reminder [2013 11 3 16 52 tz] "remind me today at 5pm to ensure dates work ok")
           {:who [:self]
            :when [2013 11 3 17 00 tz]
            :what "ensure dates work ok"
            :tags #{}}))
    (is (= (consume-reminder [2013 11 3 16 52 tz] "remind me today at 5pm, to ensure dates work ok with trailing punctuation.")
           {:who [:self]
            :when [2013 11 3 17 00 tz]
            :what "ensure dates work ok with trailing punctuation."
            :tags #{}}))
    (is (= (consume-reminder [2013 11 3 16 52 tz] "remind me to ensure parsing with punctuation after trailing times today at 5pm! #tags #too")
           {:who [:self]
            :when [2013 11 3 17 00 tz]
            :what "ensure parsing with punctuation after trailing times"
            :tags #{"tags" "too"}}))))
