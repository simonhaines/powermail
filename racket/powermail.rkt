#!/home/conamalgamate/racket/racket/bin/racket
#lang racket
(require net/mime)
(require net/head)
(require net/smtp)

; Write message to a file
(define tempfile (make-temporary-file "/home/conamalgamate/powermail/messages/msg~a"))
(with-output-to-file tempfile
  (lambda ()
    (copy-port (current-input-port) (current-output-port)))
  #:exists 'truncate)

(let* ((msg (mime-analyze (open-input-file tempfile)))
       (msg-headers (append-map extract-all-fields (cdr (message-fields msg)))))
  (let-values (((process stdout stdin stderr)
                (subprocess #f #f 'stdout
                            "/usr/bin/mail"
                            "-a" "From: Powermail <powermail@scalardata.com>"
                            "-s" (string-append "Re: " (cdr (assoc "Subject" msg-headers)))
                            (string-join (extract-addresses (cdr (assoc "From" msg-headers)) 'address) ","))))
    (display "You wrote:" stdin) (newline stdin)
    ((entity-body (message-entity msg)) stdin)
    (flush-output stdin)
    (close-output-port stdin)
    (close-input-port stdout)))