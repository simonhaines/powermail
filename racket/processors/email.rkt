#lang racket/base
(require
 racket/list
 racket/string
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
 "../reminder.rkt"
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
              (let ([commands (parse-result-semantic-value result)]
                    [tx (create-transaction user)])
                (if (> (length commands) 0)
                    ; TODO decouple templating into (template (context item record...)...)
                    (begin
                      (displayln (format "To: ~a" sender))
                      (displayln "From: powermail@scalardata.com")
                      (if (> (length commands) 1)
                          (displayln (format "Subject: ~a actions processed" (length commands)))
                          (displayln "Subject: 1 action processed"))
                      (newline)
                      (displayln "Here are the results of processing your request.")
                      (newline)
                      (for-each (lambda (command)
                                  (let ([start (first command)]
                                        [end (second command)]
                                        [item (third command)])
                                    (displayln (format "> ~a" (substring body start end)))
                                    (cond [(reminder? item)
                                           (displayln "Reminder added:")
                                           (let ([record (add-action! tx item)])
                                             (display (format "  ~a. " (caar record)))
                                             (display (format "~a: " (date->string (reminder-time item))))
                                             (displayln (reminder-content item))
                                             (when (not (null? (reminder-tags item)))
                                               (displayln (format "Tagged: ~a" (string-join (reminder-tags item) ", "))))
                                             (newline))]
                                          [else
                                           (displayln "Something else")])))
                                commands))
                    (displayln "Parsed ok, no commands")))
              (let ([err (parse-result-error result)])
                (displayln "error")
                (write err))))))))
