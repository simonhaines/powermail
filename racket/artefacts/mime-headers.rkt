#lang racket/base
(require racket/list racket/bool)
(require net/mime)
(require net/head)
(require srfi/19)

; Read mail file
(define f (open-input-file "sample.mail"))

; Parse message
(define m (mime-analyze f))

; Extract headers into association list
(define h (append-map extract-all-fields (cdr (message-fields m))))

; Extract name and address of 'From' field
(define from-name (extract-addresses (cdr (assoc "From" h)) 'name))
(define from-address (extract-addresses (cdr (assoc "From" h)) 'address))

; Extract the date of the message
(define d (string->date (cdr (assoc "Date" h)) "~a, ~d ~b ~Y ~H:~M:~S ~z"))

(display from-name)
(display from-address)
(display d)
(newline)

(define (read-email port)
  (let ((out-port (open-output-string)))
    (let loop ((line (read-line port)))
      (cond ((eq? line eof) #f)
            ((string=? (substring line 0 5) "From ")
             (open-input-string (get-output-string out-port)))
            (else
             (write-string line out-port)
             (newline out-port)
             (loop (read-line port)))))))
               
        

; Read the mbox file
(define mbox (open-input-file "guile-devel-2011-12.mbox"))
(let loop ((em (read-email mbox)))
  (if (false? em) #t
      (let* ((msg (mime-analyze em))
             (headers (append-map extract-all-fields (message-fields msg))))
        (if (eq? (length headers) 0)
            (loop (read-email mbox))
            (begin
              (display "From name: ") (display (extract-addresses (cdr (assoc "From" headers)) 'name)) (newline)
              (display "From address: ") (display (extract-addresses (cdr (assoc "From" headers)) 'address)) (newline)
              (display "To name: ") (display (extract-addresses (cdr (assoc "To" headers)) 'name)) (newline)
              (display "To address: ") (write (extract-addresses (cdr (assoc "To" headers)) 'address)) (newline)
              (display (string->date (cdr (assoc "Date" headers)) "~a, ~d ~b ~Y ~H:~M:~S ~z")) (newline)
              (newline)
              (loop (read-email mbox)))))))
