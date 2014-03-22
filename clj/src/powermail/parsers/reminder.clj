(ns powermail.parsers.reminder
  (:use powermail.core
        [powermail.parsers common datetime tag])
  (:require [dj.peg :as peg]
            [clojure.string :as string])
  (:gen-class))


(def subjects {:self "me"})

(def recipient
  (let [parsers (map (fn [[key token]]
                       (peg/alt (peg/s (literal token key) cws+)
                                (fn [[key _]] key)))
                     subjects)
        length (count parsers)]
    (cond (= length 0) (throw (Exception. "no recipients"))
          (= length 1) (first parsers)
          :else (apply peg/| parsers))))
(def recipients
  (peg/+ recipient))

(def word-boundary (peg/t #"\s+|$"))

(def word
  (peg/|
    (peg/alt (peg/s (peg/t #"my") word-boundary)
             (fn [[_ b]] (str "your" b)))
    (peg/alt (peg/s (peg/t #"i|(me)") word-boundary)
             (fn [[_ b]] (str "you" b)))
    (peg/alt (peg/s (peg/t #"[^\s#][^\s]*") word-boundary)
             (fn [w] (apply str w)))))

(def words* (peg/* word))

(def prefix
  (peg/? (peg/s (peg/t #"(that)|(about)|(to)") cws+)))

(defn content
  [^java.lang.String source success fail]
  (defn parse-datetime [source]
    (try
      (peg/parse (peg/alt (peg/s datetime (peg/t #"\p{P}*\s*") prefix words*)
                          (fn [[dt _ _ w*]] [dt (apply str w*)])) source)
      (catch Exception e nil)))
  (defn parse-word [source]
    (try
      (peg/parse word source)
      (catch Exception e nil)))
  (loop [prefix []
         src source]
    ; Try to extract a datetime and words
    (let [dt (parse-datetime src)]
      (if dt
        (success
          {:when (first (dt :result))
           :what (string/trim (apply str (conj prefix (last (dt :result)))))}
         (dt :unconsumed-input))
        (let [word (parse-word src)]
          (if word
            (recur (conj prefix (word :result)) (word :unconsumed-input))
            (fail "cannot parse content" src)))))))

(def reminder
  (peg/alt
    (peg/s (peg/t #"(remind)?\s+") recipients prefix content tags*)
    (fn [[_ r _ c t]] (assoc c :who r :tags t))))
