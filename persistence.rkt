#lang racket/base
(require
 (for-syntax racket/base)
 racket/vector
 db)

(provide
 fetch-row
 fetch-rows)

; The non-pooled connection
(define conn (virtual-connection (lambda () (sqlite3-connect #:database "powermail.db"))))

(define-syntax (fetch-rows stx)
  (syntax-case stx ()
    [(fetch-rows (stmt ...) thunk)
     #'(let ([result (query-rows conn stmt ...)])
         (if (null? result) null
             (map thunk result)))]
    [(fetch-rows stmt thunk)
     #'(let ([result (query-rows conn stmt)])
         (if (null? result) null
             (map thunk result)))]))

(define-syntax (fetch-row stx)
  (syntax-case stx ()
    [(fetch-row (stmt ...) thunk)
     #'(let ([result (query-maybe-row conn stmt ...)])
         (if result (thunk result) #f))]
    [(fetch-row stmt thunk)
     #'(let ([result (query-maybe-row conn stmt)])
         (if result (thunk result) #f))]))






#;(define (write-events! user events)
  (start-transaction conn)
  (clear-records user)
  (for ([event events])
    (let ([event-id (uuid->hex-string (make-uuid-4))])
      ; Persist event
      (query-exec conn
                  (string-append
                   "INSERT INTO Events (Id, Owner, TimeUTC, Content, Context) "
                   "VALUES ($1, $2, $3, $4, $5)")
                  event-id
                  (user-id user)
                  (date->seconds (date->tz (event-time event) "UTC"))
                  (event-content event) "None")
      ; Persist tags
      (for ([tag (event-tags event)])
        (let ([tag-id (tag->id tag)])
          (query-exec conn "INSERT INTO EventTags (Event, Tag) VALUES ($1, $2)"
                      event-id tag-id)))
      ; Record action
      (record-add-event (user-id user) event-id)))
  (commit-transaction conn))

#;(define (tag->id tag)
  (let ([result (query-maybe-row conn "SELECT Id FROM Tags WHERE Name = $1" tag)])
    (if result
        (vector-ref result 0)
        (let ([id (uuid->hex-string (make-uuid-4))])
          (query-exec conn "INSERT INTO Tags (Id, Name) VALUES ($1, $2)" id tag)
          id))))

#;(define (clear-records user)
  (query-exec conn "DELETE FROM Records WHERE Owner = $1" (user-id user)))

#;(define record-index (box 1))
#;(define (record-add-event user-id event-id)
  (let ([index (unbox record-index)])
    (query-exec conn
                "INSERT INTO Records (Owner, Number, Action, Event) VALUES ($1, $2, 'ADD', $3)"
                user-id index event-id)
    (set-box! record-index (add1 index))
    ;TODO return record structure
    ))
