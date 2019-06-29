#lang racket/base

(require
 racket/match
 "transaction.rkt"
 "reminder.rkt")

(provide
 email-template)

(define (email-template from)
   (lambda (template context item records)
    (let ([index (car record)]
          [action (cdr record)])
      (match action
        [(cons 'add (? reminder?))
         (begin
           (displayln "Reminder added:")
           (displayln (format "  ~a. ~a" index (reminder-content (cdr action)))))]))))
