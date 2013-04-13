#lang racket/base
(require
 racket/list
 (planet dvanhorn/packrat)
 (planet bzlib/date/srfi)
 (planet bzlib/date-tz)
 (planet bzlib/date-tz/util)
 "timezone.rkt"
 "common.rkt"
 "util.rkt"
 "locale.rkt")

(provide
 <time-spec>
 <time>)

(define (create-time hour minute . tz)
  (let ([new-time (list hour minute)])
    (if (null? tz)
        (append new-time (list (*tz*)))
        (if (and (not (exact-integer? (car tz)))
                 (not (and (string? (car tz))
                           (zone-exists? (car tz)))))
            (error (format "bad tz: ~s" (car tz)))
            (append new-time tz)))))

(define (utc-offset amount)
  (let-values ([(q r) (quotient/remainder amount 100)])
    (* 60 (+ (* 60 q) r))))

(define (advance-minutes m)
  (let ([new-time (seconds->date (+ (* 60 m) (date->seconds (*date-ref*))))])
    (create-time (date-hour new-time)
                 (date-minute new-time))))

(define (advance-hours h)
  (let ([new-time (seconds->date (+ (* 3600 h) (date->seconds (*date-ref*))))])
    (create-time (date-hour new-time)
                 (date-minute new-time))))

(define <space>
  (parse <space>
         (<space>
          (('#\space <space>) #t)
          (('#\space) #t))))

(define <time-spec>
  (parse <time-spec>
         (<time-spec>
          ((t := <time>) t)
          (('#\a '#\t <space> t := <time>) t)
          (('#\i '#\n <space> '#\1 <space> '#\m '#\i '#\n '#\u '#\t '#\e) (advance-minutes 1))
          (('#\i '#\n <space> d := <1or2digit> <space> '#\m '#\i '#\n '#\u '#\t '#\e '#\s) (advance-minutes d))
          (('#\i '#\n <space> '#\1 <space> '#\h '#\o '#\u '#\r) (advance-hours 1))
          (('#\i '#\n <space> d := <1or2digit> <space> '#\h '#\o '#\u '#\r '#\s) (advance-hours d)))))

(define <time>
  (parse <time>
         (<time>
          ((t := <time-base> <opt-space> z := <timezone>)
           (append (take t 2) z))
          ((t := <time-base> <opt-space> z := <timezone-abbrev>)
           (append (take t 2) (list (utc-offset (cadr z)))))
          ((t := <time-base> <opt-space> '#\+ z := <4digit>)
           (append (take t 2) (list (utc-offset z))))
          ((t := <time-base> <opt-space> '#\- z := <4digit>)
           (append (take t 2) (list (utc-offset (- 0 z)))))
          ((t := <time-base>) t))
         (<time-base>
          ((t := <time-hour-minute> <opt-space> '#\a '#\m)
           (let ((hour (car t))
                 (minute (cdr t)))
             (cond ((= hour 0) (error "hour too small"))
                   ((> hour 12) (error "hour too large"))
                   ((> minute 59) (error "minute too large"))
                   ((= hour 12) (create-time 0 minute))
                   (else (create-time hour minute)))))
          ((t := <time-hour-minute> <opt-space> '#\p '#\m)
           (let ((hour (car t))
                 (minute (cdr t)))
             (cond ((= hour 0) (error "hour too small"))
                   ((> hour 12) (error "hour too large"))
                   ((> minute 59) (error "minute too large"))
                   ((< hour 12) (create-time (+ 12 hour) minute))
                   (else (create-time hour minute)))))
          ((t := <time-hour-minute>)
           (let ((hour (car t))
                 (minute (cdr t)))
             (cond ((> hour 23) (error "hour too large"))
                   ((> minute 59) (error "minute too large"))
                   (else (create-time hour minute)))))
          ((h := <time-hour> <opt-space> '#\a '#\m)
           (cond ((= h 0) (error "hour too small"))
                 ((> h 12) (error "hour too large"))
                 ((= h 12) (create-time 0 0))
                 (else (create-time h 0))))
          ((h := <time-hour> <opt-space> '#\p '#\m)
           (cond ((= h 0) (error "hour too small"))
                 ((> h 12) (error "hour too large"))
                 ((< h 12) (create-time (+ h 12) 0))
                 (else (create-time h 0))))
          (('#\n '#\o '#\o '#\n) (create-time 12 0))
          (('#\m '#\i '#\d '#\d '#\a '#\y) (create-time 12 0))
          (('#\m '#\i '#\d '#\n '#\i '#\g '#\h '#\t) (create-time 0 0)))
         (<time-hour>
          ((h := <1digit>) h)
          ((h := <2digit>) h))
         (<time-hour-minute>
          ((h := <time-hour> '#\: m := <2digit>)
           (cond ((> h 23) (error "hour too large"))
                 ((> m 59) (error "minute too large"))
                 (else (cons h m)))))
         (<opt-space>
          ((<space>) #t)
          (() #t))))

; Time tests
(module+ test
  (require test-engine/racket-tests)
  
  (define (parse-string str)
    (parse-result-semantic-value (<time-spec> (packrat-string-results "<str>" str))))
  
  (check-expect (parse-string "10:00") (create-time 10 0))
  (check-expect (parse-string "00:00") (create-time 0 0))
  (check-expect (parse-string "13:59") (create-time 13 59))
  (check-expect (parse-string "2:50") (create-time 2 50))
  (check-expect (parse-string "0:10") (create-time 0 10))
  (check-expect (parse-string "2:50am") (create-time 2 50))
  (check-expect (parse-string "2:50pm") (create-time 14 50))
  (check-expect (parse-string "12:45pm") (create-time 12 45))
  (check-expect (parse-string "1:45 pm") (create-time 13 45))
  (check-expect (parse-string "12am") (create-time 0 0))
  (check-expect (parse-string "12pm") (create-time 12 0))
  (check-expect (parse-string "1pm") (create-time 13 0))
  (check-error (parse-string "2:60") "minute too large")
  (check-error (parse-string "25:20") "hour too large")
  (check-error (parse-string "13:01pm") "hour too large")
  (check-error (parse-string "13:01am") "hour too large")
  (check-error (parse-string "2:60am") "minute too large")
  (check-error (parse-string "2:60pm") "minute too large")
  (check-error (parse-string "0:23am") "hour too small")
  (check-error (parse-string "0:30pm") "hour too small")
  (check-expect (parse-string "noon") (create-time 12 0))
  (check-expect (parse-string "midday") (create-time 12 0))
  (check-expect (parse-string "midnight") (create-time 0 0))
  
  (check-expect (parse-string "12:45pm australia/hobart") (create-time 12 45 "Australia/Hobart"))
  (check-expect (parse-string "1:45pm aedt") (create-time 13 45 39600))
  (check-expect (parse-string "12:00 +1030") (create-time 12 0 37800))
  (check-expect (parse-string "12:00 -1145") (create-time 12 0 -42300))
  
  (test))