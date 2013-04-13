#lang racket/base
(require
 (for-syntax racket/base)
 (planet dvanhorn/packrat)
 (planet bzlib/date/srfi)
 "common.rkt"
 "util.rkt"
 "locale.rkt")  ; Preferences

(provide
 <date-spec>
 <date>)

(define (create-date year month day)
  (list year month day))

(define (advance-year month day)
  (let loop ((d (build-date (date-year (*date-ref*)) month day)))
    (if (date<? d (*date-ref*))
        (loop (build-date
               (+ 1 (date-year d))
               (date-month d)
               (date-day d)))
        (create-date (date-year d)
                     (date-month d)
                     (date-day d)))))

(define (advance-day day) ; week-day>mday
  (let loop ((d (*date-ref*)))
    (if (not (= (date-week-day d) day))
        (loop (date+ d 1))
        (create-date (date-year d)
                     (date-month d)
                     (date-day d)))))

(define (advance-days days)
  (let ([d (date+ (*date-ref*) days)])
    (create-date (date-year d)
                 (date-month d)
                 (date-day d))))

(define (format-date first-part second-part third-part)
  (cond ((eq? (*date-fmt*) 'MDY)
         (create-date third-part first-part second-part))
        ((eq? (*date-fmt*) 'DMY)
         (create-date third-part second-part first-part))
        (else (error "bad date format"))))

(define <day-of-week>
  (parse <day-of-week>
         (<day-of-week>
          (('#\s '#\u '#\n '#\d '#\a '#\y) 0)
          (('#\s '#\u '#\n) 0)
          (('#\m '#\o '#\n '#\d '#\a '#\y) 1)
          (('#\m '#\o '#\n) 1)
          (('#\t '#\u '#\e '#\s '#\d '#\a '#\y) 2)
          (('#\t '#\u '#\e) 2)
          (('#\w '#\e '#\d '#\n '#\e '#\s '#\d '#\a '#\y) 3)
          (('#\w '#\e '#\d) 3)
          (('#\t '#\h '#\u '#\r '#\s '#\d '#\a '#\y) 4)
          (('#\t '#\h '#\u) 4)
          (('#\f '#\r '#\i '#\d '#\a '#\y) 5)
          (('#\f '#\r '#\i) 5)
          (('#\s '#\a '#\t '#\u '#\r '#\d '#\a '#\y) 6)
          (('#\s '#\a '#\t) 6))))

(define <month-name>
  (parse <month-name>
         (<month-name>
          (('#\j '#\a '#\n '#\u '#\a '#\r '#\y) 1)
          (('#\j '#\a '#\n) 1)
          (('#\f '#\e '#\b '#\r '#\u '#\a '#\r '#\y) 2)
          (('#\f '#\e '#\b) 2)
          (('#\m '#\a '#\r '#\c '#\h) 3)
          (('#\m '#\a '#\r) 3)
          (('#\a '#\p '#\r '#\i '#\l) 4)
          (('#\a '#\p '#\r) 4)
          (('#\m '#\a '#\y) 5)
          (('#\j '#\u '#\n '#\e) 6)
          (('#\j '#\u '#\n) 6)
          (('#\j '#\u '#\l '#\y) 7)
          (('#\j '#\u '#\l) 7)
          (('#\a '#\u '#\g '#\u '#\s '#\t) 8)
          (('#\a '#\u '#\g) 8)
          (('#\s '#\e '#\p '#\t '#\e '#\m '#\b '#\e '#\r) 9)
          (('#\s '#\e '#\p) 9)
          (('#\o '#\c '#\t '#\o '#\b '#\e '#\r) 10)
          (('#\o '#\c '#\t) 10)
          (('#\n '#\o '#\v '#\e '#\m '#\b '#\e '#\r) 11)
          (('#\n '#\o '#\v) 11)
          (('#\d '#\e '#\c '#\e '#\m '#\b '#\e '#\r) 12)
          (('#\d '#\e '#\c) 12))))

(define <date-spec>
  (parse <date-spec>
         (<date-spec>
          ((d := <date>) d)
          (('#\o '#\n <whitespace> d := <date>) d)
          (('#\o '#\n <whitespace> d := <day-of-week>) (advance-day d))
          (('#\i '#\n <whitespace> m := <month-name>) (advance-year m 1))
          (('#\i '#\n <whitespace> '#\1 <whitespace> '#\d '#\a '#\y) (advance-days 1))
          (('#\i '#\n <whitespace> d := <1or2digit> <whitespace> '#\d '#\a '#\y '#\s) (advance-days d))
          (('#\i '#\n <whitespace> '#\1 <whitespace> '#\w '#\e '#\e '#\k) (advance-days 7))
          (('#\i '#\n <whitespace> d := <1or2digit> <whitespace> '#\w '#\e '#\e '#\k '#\s) (advance-days (* d 7)))
          (('#\n '#\e '#\x '#\t <whitespace> d := <day-of-week>) (advance-day d))
          (('#\n '#\e '#\x '#\t <whitespace> m := <month-name>) (advance-year m 1))
          (('#\t '#\h '#\i '#\s <whitespace> d := <day-of-week>) (advance-day d))
          (('#\t '#\h '#\i '#\s <whitespace> m := <month-name>) (advance-year m 1))
          (('#\t '#\o '#\d '#\a '#\y) (create-date (date-year (*date-ref*)) (date-month (*date-ref*)) (date-day (*date-ref*))))
          (('#\t '#\o '#\m '#\o '#\r '#\r '#\o '#\w)
           (let ((tomorrow (date+ (*date-ref*) 1)))
             (create-date (date-year tomorrow) (date-month tomorrow) (date-day tomorrow))))
          ((d := <day-of-week>) (advance-day d)))))

(define <date>
  (parse <date>
         (<date>
          ((n := <day-of-week> <cws> m := <month-name> <cws> d := <day-number> <cws> y := <year>) (create-date y m d))
          ((m := <month-name> <cws> d := <day-number> <cws> y := <year>) (create-date y m d))
          ((n := <day-of-week> <cws> m := <month-name> <cws> d := <day-number>) (advance-year m d))
          ((m := <month-name> <whitespace> d := <day-number>) (advance-year m d))
          ((n := <day-of-week> <cws> d := <day-number> <cws> m := <month-name> <cws> y := <year>) (create-date y m d))
          ((d := <day-number> <whitespace> m := <month-name> <cws> y := <year>) (create-date y m d))
          ((n := <day-of-week> <cws> d := <day-number> <cws> m := <month-name>) (advance-year m d))
          ((d := <day-number> <whitespace> m := <month-name>) (advance-year m d))
          ((d := <slash-date>) d)
          ((d := <dash-date>) d)
          ((d := <dot-date>) d))
         (<day-number>
          ((d := <1or2digit> '#\s '#\t) d)
          ((d := <1or2digit> '#\n '#\d) d)
          ((d := <1or2digit> '#\r '#\d) d)
          ((d := <1or2digit> '#\t '#\h) d)
          ((d := <1or2digit>) d))
         (<year>
          ((y := <2digit>) (+ 2000 y))
          ((y := <4digit>) y))
         (<slash-date>
          ((f := <1or2digit> '#\/ s := <1or2digit> '#\/ y := <2or4digit>) (format-date f s y))
          ((f := <1or2digit> '#\/ s := <1or2digit>) (format-date f s (date-year (*date-ref*)))))
         (<dot-date>
          ((y := <2or4digit> '#\. m := <1or2digit> '#\. d := <1or2digit>) (create-date y m d)))
         (<dash-date>
          ((y := <2or4digit> '#\- m := <1or2digit> '#\- d := <1or2digit>) (create-date y m d)))
         (<cws>  ; Comma whitespace
          ((<whitespace> <cws>) #t)
          (('#\, <cws>) #t)
          ((<whitespace>) #t)
          (('#\,) #t))))

(module+ test
  (require test-engine/racket-tests)
  
  (define (parse-string str)
    (parse-result-semantic-value (<date> (packrat-string-results "<str>" str))))
  (define (parse-prefixed-string str)
    (parse-result-semantic-value (<date-spec> (packrat-string-results "<str>" str))))
  
  (define-syntax (check-date-spec-ref stx)
    (syntax-case stx ()
      ((_ (y m d) str (ey em ed))
       #'(check-expect
          (parameterize ((*date-ref* (build-date y m d)))
            (parse-prefixed-string str))
          (create-date ey em ed)))))
  (define-syntax (check-date-ref stx)
    (syntax-case stx ()
      ((_ (y m d) str (ey em ed))
       #'(check-expect
          (parameterize ((*date-ref* (build-date y m d)))
            (parse-string str))
          (create-date ey em ed)))))
  (define-syntax (check-date-fmt stx)
    (syntax-case stx ()
      ((_ fmt str (ey em ed))
       #'(check-expect
          (parameterize ((*date-fmt* fmt))
            (parse-string str))
          (create-date ey em ed)))))
  
  (check-date-spec-ref (2013 2 24) "today" (2013 2 24))
  (check-date-spec-ref (2013 2 28) "tomorrow" (2013 3 1))
  (check-date-spec-ref (2012 2 28) "tomorrow" (2012 2 29))
  (check-date-spec-ref (2013 2 24) "next monday" (2013 2 25))
  (check-date-spec-ref (2013 2 24) "on friday" (2013 3 1))
  (check-date-spec-ref (2013 2 24) "this sat" (2013 3 2))
  (check-date-spec-ref (2013 2 24) "next march" (2013 3 1))
  (check-date-spec-ref (2013 2 24) "this jan" (2014 1 1))
  (check-date-spec-ref (2013 3 23) "tuesday" (2013 3 26))
  (check-date-spec-ref (2013 3 27) "tue" (2013 4 2))
  (check-date-spec-ref (2013 3 23) "in march" (2014 3 1))
  (check-date-spec-ref (2013 1 23) "in march" (2013 3 1))
  (check-date-spec-ref (2013 4 3) "in 1 day" (2013 4 4))
  (check-date-spec-ref (2013 4 3) "in 5 days" (2013 4 8))
  (check-date-spec-ref (2013 4 3) "in 1 week" (2013 4 10))
  (check-date-spec-ref (2013 4 3) "in 5 weeks" (2013 5 8))
  
  (check-date-ref (2013 2 24) "sunday, feb 24, 2013" (2013 2 24))
  (check-date-ref (2013 2 24) "saturday, feb 24, 2013" (2013 2 24)) ; Day names are ignored in fully-qualified dates
  (check-date-ref (2013 2 24) "february 24 2013" (2013 2 24))
  (check-date-ref (2013 2 24) "feb   24,   2013" (2013 2 24))
  (check-date-ref (2013 2 24) "saturday, jan 24" (2014 1 24))
  (check-date-ref (2013 2 24) "march 23rd" (2013 3 23))
  (check-date-ref (2013 2 24) "sunday, 24th february, 2013" (2013 2 24))
  (check-date-ref (2013 2 24) "saturday 24 feb 2013" (2013 2 24))
  (check-date-ref (2013 2 24) "24 february 2013" (2013 2 24))
  (check-date-ref (2013 2 24) "saturday 24 feb" (2013 2 24))
  (check-date-ref (2013 2 24) "24th february" (2013 2 24))
  (check-date-ref (2013 2 24) "23rd february" (2014 2 23))
  (check-date-ref (2013 2 24) "2013-10-26" (2013 10 26))
  (check-date-ref (2013 2 24) "2013.10.26" (2013 10 26))
  
  (check-date-fmt 'MDY "2/24/2013" (2013 2 24))
  (check-date-fmt 'DMY "24/2/2013" (2013 2 24))
  
  (test))