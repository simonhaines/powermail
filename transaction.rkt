#lang racket/base
(require
 "user.rkt"
 (prefix-in reminder: "reminder.rkt")  ; TODO do better
 (prefix-in db: "persistence.rkt"))

(provide
 create
 add!
 delete!
 ; rollback!
 ; commit)
 )

(struct transaction
  (user last (next #:mutable)) #:transparent)
(struct record
  (action reminder contact (rolled-back #:mutable #:auto)))

(define (create user)
  (transaction user (last-transaction (user-id user)) '()))

(define (add! transaction . items)
  (map (lambda (item)
         (append-transaction transaction (create-record 'add item))
         (cons (length (transaction-next transaction)) item))
       items))

(define (delete! transaction . items)
  (map (lambda (item)
         (append-transaction transaction (create-record 'del item))
         (cons (length (transaction-next transaction)) item))
       items))

(define (rollback! transaction . indeces)
  (map (lambda (index)
         (let ([record (list-ref (transaction-last transaction) index)])
           (append-transaction transaction (rollback-record record))))
       indeces))

(define (append-transaction transaction record)
  (set-transaction-next! transaction
                         (append (transaction-next transaction)
                                 (list record))))

(define (create-record type item)
  (cond
    [(reminder:reminder? item) (record type (reminder:reminder-id item))]
    [else (error "unknown type")]))

(define (rollback-record record)
  (set-record-rolled-back! record #t)
  (record
   (case (record-action record)
     [('add) 'del]
     [('del) 'add]
     [else (record-action record)])
   (record-reminder record)
   (record-contact record)))

(define (last-transaction user-id)
  (db:fetch-rows
   ("SELECT Action, Event, Contact FROM Records WHERE Owner = $1 ORDER BY Number" user-id)
   (lambda (row)
     (record
      (string->symbol (vector-ref row 0))
      (vector-ref row 1)
      (vector-ref row 2)))))
