#lang racket/base

(require
 "persistence.rkt")

(provide
 (struct-out user)
 lookup-user)

(struct user
  (id date-format tz time-ref)
  #:transparent)

; Returns either a user or false
(define (lookup-user email)
  (fetch-row
   ((string-append
     "SELECT Users.Id, Users.DateFormat, Users.Timezone, Users.TimeRef "
     "FROM Users INNER JOIN Contacts ON Contacts.Owner = Users.Id "
     "WHERE Contacts.Email = $1") email)
   (lambda (row)
     (user (vector-ref row 0)
          (string->symbol (vector-ref row 1))
          (vector-ref row 2)
          (let-values ([(hour minute) (quotient/remainder (vector-ref row 3) 100)])
            (list hour minute))))))