(ns powermail.parsers.tag
  (:use powermail.core
        [powermail.parsers common])
  (:require [dj.peg :as peg])
  (:gen-class))

(def tag-chars
  (peg/t #"[^\s,#]+"))

(def tag
  (peg/alt (peg/s (literal "#") tag-chars cws*)
    (fn [[_ c _]] c)))

(def tags*
  (peg/alt (peg/* tag)
           (fn [t] (set t))))
