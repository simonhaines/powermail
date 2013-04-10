#lang racket/base
(require
 (planet dvanhorn/packrat)
 (planet bzlib/date/srfi)
 "date.rkt"
 "time.rkt"
 "common.rkt"
 "util.rkt"
 "locale.rkt")  ; Preferences

(define (date-ref->date)
  (list (date-year (*date-ref*))
        (date-month (*date-ref*))
        (date-day (*date-ref*))))

(define <duration-spec>
  (parse <duration-spec>
         (<duration-spec>
          (('#\f '#\r '#\o '#\m d := <duration>) d)
          ((d := <duration> d)))))

(define <duration>
  (parse <duration>
         (<duration>
          ((s := <duration-base> <duration-span> <duration-base>) s)
          ((d := <date-time-duration>) d)
          ((d := <date-duration>) d)
          ((t := <time-duration>) t))
         (<duration-base>
          ((d := <date> <comma-whitespace> t := <time>) (append d t))
          ((t := <time> <comma-whitespace> d := <date>) (append d t)))
         (<date-time-duration>
          ((d := <date> <comma-whitespace> t := <time-duration>) (append d t))
          ((t := <time-duration> <comma-whitespace> d := <date>) (append d t)))
         (<date-duration>
          ((n := <1or2digit> <duration-span> d := <date>) (append (take d 2) (list n))))
         (<time-duration>
          ((h := <1or2digit> <duration-span> t := <time>) (append h (cdr t))))
         (<duration-span>
          ((<opt-space> '#\- <opt-space>) #t)
          ((<whitespace> '#\t '#\o <whitespace>) #t)
          ((<whitespace> '#\u '#\n '#\t '#\i '#\l <whitespace>) #t))
         (<comma-whitespace>
          (('#\, <comma-whitespace>) #t)
          ((<whitespace> <comma-whitespace>) #t))
         (<opt-space>
          ((<space>) #t)
          (() #t))))