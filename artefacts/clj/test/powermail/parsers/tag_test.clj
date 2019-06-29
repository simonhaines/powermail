(ns powermail.parsers.tag-test
  (:require [clojure.test :refer :all]
            [powermail.parsers.tag :refer :all]
            [dj.peg :as peg]))


(defn consume-all
  [parser input]
  (let [result (peg/parse parser input)]
    (if (= (result :unconsumed-input) "")
      (result :result) result)))

(defn consume-tags
  [input]
  (consume-all tags* input))

(deftest test-tag
  (testing "tag parser"
    (is (thrown-with-msg? Exception #"Parse failed" (peg/parse tag "not a tag")))
    (is (= (consume-tags "#tag") #{"tag"}))
    (is (= (consume-tags "#tag1 #tag2 #tag2") #{"tag1" "tag2"}))
    (is (= (consume-tags "#tag1, #tag2#tag3") #{"tag1" "tag2" "tag3"}))))
