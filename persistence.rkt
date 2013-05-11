#lang racket/base
(require
 db
 (planet williams/uuid/uuid))

(provide 
 lookup-user
 (struct-out user))

; The non-pooled connection
(define conn (virtual-connection (lambda () (sqlite3-connect #:database "powermail.db"))))

(struct user (id date-format tz time-ref)
  #:transparent)

(define (lookup-user email)
  (let ([result (query-maybe-row conn
                                 (string-append
                                  "SELECT Users.Id, Users.DateFormat, Users.Timezone, Users.TimeRef "
                                  "FROM Users INNER JOIN Contacts ON Contacts.Owner = Users.Id "
                                  "WHERE Contacts.Email = $1") email)])
    (if result
        (user (hex-string->uuid (vector-ref result 0))
              (string->symbol (vector-ref result 1))
              (vector-ref result 2)
              (let-values ([(hour minute) (quotient/remainder (vector-ref result 3) 100)])
                (list hour minute)))
        null)))
