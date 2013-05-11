#lang racket/base
(require
 net/mime
 "parsers/locale.rkt")

; Analyse the email message on stdin and lookup the sender
(let* ([msg (mime-analyze (current-input-port))]
       [headers (append-map extract-all-fields (message-fields msg))]
       [sender (extract-addresses (cdr (assoc "From" headers)) 'address)]
       [user (lookup-user sender)])
  ; If the sender is not a user, don't do any further processing
  (when (not (null? user))
    (parameterize ([*date-fmt* (user-date-format)]
                   [*tz* (user-tz)]
                   [*date-ref* (date->tz (current-date) (*tz*))]uuid
  ; TODO resolve user's contacts