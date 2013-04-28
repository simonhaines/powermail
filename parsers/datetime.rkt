#lang racket/base
(require
 (for-syntax racket/base)
 (planet dvanhorn/packrat)
 racket/date
 (planet bzlib/date-tz/plt)
 "date.rkt"
 "time.rkt"
 "duration.rkt"
 "common.rkt"
 "locale.rkt")
(provide
 <datetime-spec>
 <datetime>)

; Fold the list structures produced by the <date> and <time> parsers
; into a Racket date structure
(define (date->datetime date)
  (let ([time (*time-ref*)])
    (build-date/tz (list-ref date 0) (list-ref date 1) (list-ref date 2)
                   (list-ref time 0) (list-ref time 1) #:tz (*tz*))))
(define (time->datetime time)
  (let ([date (*date-ref*)])
    (build-date/tz (date-year date) (date-month date) (date-day date)
                   (list-ref time 0) (list-ref time 1) #:tz (*tz*))))
(define (date-time->datetime date time)
  (build-date/tz (list-ref date 0) (list-ref date 1) (list-ref date 2)
                 (list-ref time 0) (list-ref time 1) #:tz (*tz*)))
(define (datetime->datetime dt)
  (build-date/tz (list-ref dt 0) (list-ref dt 1) (list-ref dt 2)
                 (list-ref dt 3) (list-ref dt 4) #:tz (list-ref dt 5)))

(define (advance-minutes m)
  (let ([new-time (seconds->date (+ (* 60 m) (date->seconds (*date-ref*))))])
    (date-time->datetime
     (list (date-year new-time)
           (date-month new-time)
           (date-day new-time))
     (list (date-hour new-time)
           (date-minute new-time)
           (*tz*)))))

(define (advance-hours h)
  (let ([new-time (seconds->date (+ (* 3600 h) (date->seconds (*date-ref*))))])
    (date-time->datetime
     (list (date-year new-time)
           (date-month new-time)
           (date-day new-time))
     (list (date-hour new-time)
           (date-minute new-time)
           (*tz*)))))

(define <datetime-spec>
  (parse <datetime-spec>
         (<datetime-spec>
          ((d := <datetime>) d)
          (('#\i '#\n <whitespace+> d := <1or2digit> <whitespace+> '#\m '#\i '#\n '#\u '#\t '#\e '#\s) (advance-minutes d))
          (('#\i '#\n <whitespace+> '#\1 <whitespace+> '#\m '#\i '#\n '#\u '#\t '#\e) (advance-minutes 1))
          (('#\i '#\n <whitespace+> d := <1or2digit> <whitespace+> '#\h '#\o '#\u '#\r '#\s) (advance-hours d))
          (('#\i '#\n <whitespace+> '#\1 <whitespace+> '#\h '#\o '#\u '#\r) (advance-hours 1)))))

(define <datetime>
  (parse <datetime>
         (<datetime>
          ((d := <duration>) (datetime->datetime d))
          ((d := <date-spec> <cws+> t := <time-spec>) (date-time->datetime d t))
          ((t := <time-spec> <cws+> d := <date-spec>) (date-time->datetime d t))
          ((d := <date-spec>) (date->datetime d))
          ((t := <time-spec>) (time->datetime t)))
         (<cws+>
          ((w := <whitespace+> c := <cws*>) (string-append w c))
          (('#\, c := <cws*>) (string-append "," c)))
         (<cws*>
          ((c := <cws+>) c)
          (() ""))))

(module+ test
  (require
   (planet bzlib/date/plt)
   (planet bzlib/date-tz/plt)
   test-engine/racket-tests
   "util.rkt")
  
  (define-syntax (check-date-ref stx)
    (syntax-case stx ()
      ((_ (date* ...) parser str (expected-date* ...))
       #'(check-expect
          (parameterize ((*date-ref* (build-date/tz date* ... #:tz (*tz*))))
            (parse-all parser str))
          (datetime->datetime (list expected-date* ... (*tz*)))))))
  
  (check-date-ref (2013 4 27) <datetime-spec> "tomorrow 5pm" (2013 4 28 17 0))
  (check-date-ref (2013 4 27 17 6) <datetime-spec> "in 5 minutes" (2013 4 27 17 11))
  (check-date-ref (2013 4 27 17 50) <datetime-spec> "in 20 minutes" (2013 4 27 18 10))
  (check-date-ref (2013 4 27 23 50) <datetime-spec> "in 20 minutes" (2013 4 28 0 10))
  (check-date-ref (2013 4 27) <datetime-spec> "tomorrow,, at 5pm" (2013 4 28 17 0))
  (check-date-ref (2013 4 27 12) <datetime-spec> "5-9pm" (2013 4 27 17 0))
  (check-date-ref (2013 4 27 10) <datetime-spec> "5pm-9pm" (2013 4 27 17 0))
  (check-date-ref (2013 4 27 17) <datetime-spec> "5pm-9" (2013 4 27 17 0))
  (check-date-ref (2013 4 27 10) <datetime-spec> "5am-9pm" (2013 4 27 5 0))
  (check-date-ref (2013 4 28) <datetime-spec> "28/4, 10am" (2013 4 28 10 0))
  
  (test))

