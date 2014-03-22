(ns powermail.parsers.common
  (:refer-clojure)
  (:require [dj.peg :as peg]))


(defn literal
  "Matches a literal string with a result value"
  ([^java.lang.String token]
   (literal token token))
  ([^java.lang.String token value]
   (fn [source success fail]
     (if (.startsWith source token)
       (success value (subs source (count token)))
       (fail token source)))))

(def ws+
   (peg/t #"\s+"))

(def ws*
   (peg/t #"\s*"))

(def cws+
  (peg/t #"[,\s]+"))

(def cws*
  (peg/t #"[,\s]*"))

(def two-digit
  (peg/alt
   (peg/t #"\d{2}(?!\d)") #(Integer/parseInt %)))

(def one-or-two-digit
  (peg/alt
   (peg/t #"\d{1,2}(?!\d)") #(Integer/parseInt %)))

(def four-digit
  (peg/alt
   (peg/t #"\d{4}(?!\d)") #(Integer/parseInt %)))

(def two-or-four-digit
  (peg/| four-digit two-digit))

(def digits+
  (peg/alt (peg/t #"\d+") #(Integer/parseInt %)))
