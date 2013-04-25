#lang racket/base
(require
 (for-syntax racket/base)
 (planet dvanhorn/packrat)
 "date.rkt"
 "time.rkt"
 "duration.rkt"
 "common.rkt"
 "locale.rkt")
(provide
 <datetime>)

(define (build-date datetime) datetime)

(define <datetime>
  (parse <datetime>
         (<datetime>
          ((d := <duration>) (build-date d))
          ((d := <date-spec> <cws+> t := <time-spec>) (build-date (append d t)))
          ((t := <time-spec> <cws+> d := <date-spec>) (build-date (append d t))))
         (<cws+>
          ((w := <whitespace+> c := <cws*>) (string-append w c))
          (('#\, c := <cws*>) (string-append "," c)))
         (<cws*>
          ((c := <cws+>) c)
          (() ""))))

(module+ test
  (require
   test-engine/racket-tests
   "util.rkt")
  
  (check-expect (parse-all <datetime> "tomorrow,, at 5pm") '(2013 4 26 17 0 "Australia/Hobart"))
  (check-expect (parse-all <datetime> "5-9pm") '(2013 4 25 17 0 "Australia/Hobart"))
  (check-expect (parse-all <datetime> "5pm-9pm") '(2013 4 25 17 0 "Australia/Hobart"))
  (check-expect (parse-all <datetime> "5pm-9") '(2013 4 25 17 0 "Australia/Hobart"))
  (check-expect (parse-all <datetime> "5am-9pm") '(2013 4 25 5 0 "Australia/Hobart"))
  
  (test))

