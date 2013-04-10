#lang racket/base
(require
 (planet dvanhorn/packrat)
 "util.rkt")
(provide
 <digit>
 <1digit>
 <2digit>
 <4digit>
 <1or2digit>
 <2or4digit>
 <whitespace>)

(define <digit>
  (parse <digit>
         (<digit>
          ((d := (? char-numeric?)) d))))

(define <1digit>
  (parse <1digit>
         (<1digit>
          ((d := <digit> (! <digit>)) (chars->num d)))))

(define <2digit>
  (parse <2digit>
         (<2digit>
          ((d1 := <digit> d2 := <digit> (! <digit>)) (chars->num d1 d2)))))

(define <4digit>
  (parse <4digit>
         (<4digit>
          ((d1 := <digit> d2 := <digit> d3 := <digit> d4 := <digit> (! <digit>)) (chars->num d1 d2 d3 d4)))))

(define <1or2digit>
  (parse <1or2digit>
         (<1or2digit>
          ((d := <1digit>) d)
          ((d := <2digit>) d))))

(define <2or4digit>
  (parse <2or4digit>
         (<2or4digit>
          ((d := <2digit>) d)
          ((d := <4digit>) d))))

(define <whitespace>
  (parse <whitespace>
         (<whitespace>
          ((w := (? char-whitespace?) <whitespace>) #t)
          ((w := (? char-whitespace?)) #t))))


