#lang racket/base
(require
 racket/list
 net/mime
 net/head
 (planet bzlib/date/plt)
 (planet bzlib/date-tz/plt)
 (planet dvanhorn/packrat)
 "../persistence.rkt"
 "../parsers/email.rkt"
 "../parsers/locale.rkt"
 "../parsers/util.rkt"
 "../user.rkt"
 "../transaction.rkt")

; Analyse the email message on stdin and lookup the sender
(let* ([msg (mime-analyze (current-input-port))]
       [headers (append-map extract-all-fields (message-fields msg))]
       [sender (car (extract-addresses (cdr (assoc "From" headers)) 'address))]
       [user (lookup-user sender)])
  ; If the sender is not a user, don't do any further processing
  (when user
    (parameterize ([*date-fmt* (user-date-format user)]
                   [*tz* (user-tz user)]
                   [*date-ref* (date->tz (current-date) (user-tz user))]
                   [*time-ref* (user-time-ref user)])
      ; TODO lookup contacts
      ; Read message body, create generator and parse message
      (let ([buffer (open-output-string)])
        ((entity-body (message-entity msg)) buffer)
        (let* ([body (get-output-string buffer)]
               [generator (string/downcase-generator body)]
               [result (<message-body> generator)])
          (if (parse-result-successful? result)
              (let ([items (parse-result-semantic-value result)]
                    [tx (create-transaction user)])
                (if (> (length events) 0)
                    (for-each (lambda (event)
                                (template (add-record! tx event)))
                              events)
                    (begin
                      (displayln "parse success, no events"))))
              (let ([err (parse-result-error result)])
                (displayln "error")
                (write err))))))))
