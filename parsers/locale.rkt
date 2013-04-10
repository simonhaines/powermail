#lang racket/base
(require
  (planet bzlib/date/srfi))

(provide
 *date-fmt*
 *date-ref*
 *tz*)

; Date format: 'MDY = month/day/year, 'DMY = day, month, year
(define *date-fmt* (make-parameter 'MDY))

; Reference date: context for calculating date offsets
(define *date-ref* (make-parameter (current-date)))

; Current timezone, see 'timezone.rkt'
(define *tz* (make-parameter "Australia/Hobart"))

; Default time, for when no time is specified
(define *time-ref* (make-parameter (list 9 0))) ; 9am