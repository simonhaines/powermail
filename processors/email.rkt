#lang racket/base
(require
 racket/list
 net/mime
 net/head
 (planet bzlib/date/plt)
 (planet bzlib/date-tz)
 "../persistence.rkt"
 "../parsers/locale.rkt")

; Analyse the email message on stdin and lookup the sender
(let* ([msg (mime-analyze (current-input-port))]
       [headers (append-map extract-all-fields (message-fields msg))]
       [sender (extract-addresses (cdr (assoc "From" headers)) 'address)]
       [user (lookup-user sender)])
  ; If the sender is not a user, don't do any further processing
  (when user)
    (parameterize ([*date-fmt* (user-date-format user)]
                   [*tz* (user-tz user)]
                   [*date-ref* (date->tz (current-date) (user-tz user))]
                   [*time-ref* (user-time-ref user)])
      ; TODO lookup contacts
      '())))
