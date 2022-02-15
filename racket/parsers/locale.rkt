#lang racket/base
(require
 (planet bzlib/date/plt)
 (planet bzlib/date-tz/plt))

(provide
 *date-fmt*
 *date-ref*
 *time-ref*
 *tz*)

; Date format: 'MDY = month/day/year, 'DMY = day, month, year
(define *date-fmt* (make-parameter 'DMY))

; Current timezone, see 'timezone.rkt'
(define *tz* (make-parameter "Australia/Hobart"))

; Reference date: context for calculating date offsets
(define *date-ref* (make-parameter (date->tz (current-date) (*tz*))))

; Default time, for when no time is specified
(define *time-ref* (make-parameter (list 9 0))) ; 9am