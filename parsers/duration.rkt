#lang racket/base
(require
 racket/list
 (planet dvanhorn/packrat)
 (planet bzlib/date/srfi)
 "date.rkt"
 "time-new.rkt"
 "common.rkt"
 "util.rkt"
 "locale.rkt")  ; Preferences

(define (create-datetime date time)
  (append date time))

(define (date-ref->date)
  (list (date-year (*date-ref*))
        (date-month (*date-ref*))
        (date-day (*date-ref*))))

(define (time-ref->time)
  (append (*time-ref*) (list (*tz*))))

(define <duration-spec>
  (parse <duration-spec>
         (<duration-spec>
          (('#\f '#\r '#\o '#\m <whitespace> d := <duration>) d)
          ((d := <duration>) d))))

(define <duration>
  (parse <duration>
         (<duration>
          ((s := <duration-base> <duration-span> <duration-base>) s)
          ((d := <date-time-duration>) d)
          ((d := <date-duration>) (create-datetime d (time-ref->time)))
          ((t := <time-duration>) (create-datetime (date-ref->date) t)))
         (<duration-base>
          ((d := <date-spec> <comma-whitespace> t := <time-spec>) (append d t))
          ((t := <time-spec> <comma-whitespace> d := <date-spec>) (append d t)))
         (<date-time-duration>
          ((d := <date-spec> <comma-whitespace> t := <time-duration>) (append d t))
          ((t := <time-duration> <comma-whitespace> d := <date-spec>) (append d t)))
         (<date-duration>
          ((n := <1or2digit> <duration-span> d := <date>) (append (take d 2) (list n)))
          ((d := <date-spec> <duration-span> <date-spec>) d))
         (<time-duration>
          ((h := <1or2digit> <duration-span> t := <time>) (cons h (cdr t)))
          ((t := <time-spec> <duration-span> <time-spec>) t))
         (<duration-span>
          ((<opt-space> '#\- <opt-space>) #t)
          ((<whitespace> '#\t '#\o <whitespace>) #t)
          ((<whitespace> '#\u '#\n '#\t '#\i '#\l <whitespace>) #t))
         (<comma-whitespace>
          (('#\, <opt-space>) #t)
          ((<whitespace> <opt-space>) #t))
         (<opt-space>
          ((<whitespace> <opt-space>) #t)
          (() #t))))

(module+ test
  (require test-engine/racket-tests)
  
  (define (parse-string str)
    (let* ([result (<duration-spec> (packrat-string-results "<str>" str))]
           [amount (or (and (parse-result-successful? result)
                            (parse-position-column (parse-results-position (parse-result-next result))))
                       0)]
           [length (string-length str)])
      (when (not (= amount length))
        (error (format "unparsed: ~s" (substring str amount))))
      (parse-result-semantic-value result)))
  
  (check-expect (parse-string "13/4/2013 10-12pm") '(2013 4 13 10 0 "Australia/Hobart"))
  (check-expect (parse-string "10-12pm 13/4/2013") '(2013 4 13 10 0 "Australia/Hobart"))
  (check-expect (parse-string "3rd jan 2014 until tomorrow") '(2014 1 3 9 0 "Australia/Hobart"))
  
  (test))
