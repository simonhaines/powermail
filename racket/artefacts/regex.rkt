#lang racket

(define-syntax match-every-working
  (syntax-rules ()
    ((_ ((re kons) more ...) knil kstr)
     (begin
       (display re)(newline)
       (display kons)(newline)
       (match-every-working (more ...) knil kstr)))
    ((_ () knil kstr)
     knil)))

(define (splice str pos . replacement)
  (string-append
   (substring str 0 (car pos))
   (if (null? replacement) ""
       (car replacement))
   (substring str (cdr pos) (string-length str))))

(define (capture str pos)
  (substring str (car pos) (cdr pos)))

; Internal definition of match-every and match/pos-every
(define-syntax match/*-every
  (syntax-rules ()
    ((_ matcher ((re func) more ...) state)
     (lambda (str)
       (match-every matcher ((re func) more ...) state str)))
    ((_ matcher ((re func) more ...) state str)
     (let ((match (matcher re str)))
       (if match
           (let-values (((new-str new-state) (func match str state)))
             (cond ((eq? new-str #t) (match/*-every matcher (more ...) new-state str))
                   ((string? new-str) (match/*-every matcher (more ...) new-state new-str))
                   (else (begin (display (format "result ~a" new-str)) (error "function must return a string")))))
           (values #f #f))))
    ((_ matcher () state str)
     (values str state))))

; (match-every ((regex func) ...) initial-state)
; Returns a function that takes a string and tests every regex
; against the string. When a match is found func is called with the
; match and initial state arguments. The func is expected to return
; two values: a new string for subsequent matches or #t to reuse the
; original string unmodified, and a modified state which is passed
; to all subsequent funcs.
; If func returns #f as a string value, the entire match-every function
; fails.
(define-syntax match-every
  (syntax-rules ()
    ((_ ((re func) more ...) state)
     (lambda (str)
       (match-every ((re func) more ...) state str)))
    ((_ ((re func) more ...) state str)
     (match/*-every regexp-match ((re func) more ...) state str))))

; (match/pos-every ((regex func) ...) initial-state)
; Like match-every except using regexp-match-positions to perform the match
(define-syntax match/pos-every
  (syntax-rules ()
    ((_ ((re func) more ...) state)
     (lambda (str)
       (match/pos-every ((re func) more ...) state str)))
    ((_ ((re func) more ...) state str)
     (match/*-every regexp-match-positions ((re func) more ...) state str))))

; (match-every* ((regex func) ...) initial-state)
; Returns a function that takes a string and repeatedly calls match-every
; with that string until it fails. If the call to match-every
; fails the first time, result is #f. If at least one call to match-every
; succeeds, returns the result of the lasst successful call.
(define-syntax match-every*
  (syntax-rules ()
    ((_ ((re func) more ...) state)
     (lambda (str)
       (define (consume recursed consumed-str consumed-state)
         (let-values (((new-str new-state)
                       (match-every ((re func) more ...) consumed-state consumed-str)))
           (cond ((or
                   (eq? new-str #t)
                   (and (string? new-str)
                        (string=? new-str consumed-str)))
                  (error "error: match-every* functions must return a modified string"))
                 ((eq? new-str #f)
                  (if recursed
                      (values consumed-str consumed-state)
                      (values #f #f)))
                 (else (consume #t new-str new-state)))))
       (consume #f str state)))))

(define-syntax match/pos-every*
  (syntax-rules ()
    ((_ ((re func) more ...) state)
     (lambda (str)
       (define (consume recursed consumed-str consumed-state)
         (let-values (((new-str new-state)
                       (match/pos-every ((re func) more ...) consumed-state consumed-str)))
           (cond ((or
                   (eq? new-str #t)
                   (and (string? new-str)
                        (string=? new-str consumed-str)))
                  (error "error: match/pos-every* functions must return a modified string"))
                 ((eq? new-str #f)
                  (if recursed
                      (values consumed-str consumed-state)
                      (values #f #f)))
                 (else (consume #t new-str new-state)))))
       (consume #f str state)))))

; (match-any ((regex func) ...) initial-state)
; Like match-every except all regexes are tried until one succeeds.
(define-syntax match-any
  (syntax-rules ()
    ((_ ((re func) more ...) state)
     (lambda (str)
       (match-any ((re func) more ...) state str)))
    ((_ ((re func) more ...) state str)
     (match/*-any regexp-match ((re func) more ...) state str))))

(define-syntax match/pos-any
  (syntax-rules ()
    ((_ ((re func) more ...) state)
     (lambda (str)
       (match/pos-any ((re func) more ...) state str)))
    ((_ ((re func) more ...) state str)
     (match/*-any regexp-match-positions ((re func) more ...) state str))))

(define-syntax match/*-any
  (syntax-rules ()
    ((_ matcher ((re func) more ...) state)
     (lambda (str)
       (match/*-any matcher ((re func) more ...) state str)))
    ((_ matcher ((re func) more ...) state str)
     (let ((match (matcher re str)))
       (if match
           (let-values (((new-str new-state) (func match str state)))
             (cond ((eq? new-str #t) (values str new-state))
                   ((string? new-str) (values new-str new-state))
                   (else (error "function must return a string"))))
           (match/*-any matcher (more ...) state str))))
    ((_ matcher () state str)
     (values #f #f))))

; Recursive version of match-any
(define-syntax match-any*
  (syntax-rules ()
    ((_ ((re func) more ...) state str)
     (let consume ((recursed #f)
                   (consumed-str str)
                   (consumed-state state))
       (let-values (((new-str new-state)
                     (match-any ((re func) more ...) consumed-state consumed-str)))
         (display (format "new-str: ~a" new-str))(newline)
         (if (eq? new-str #f)
             (if recursed
                 (values consumed-str consumed-state)
                 (values #f #f))
             (consume #t new-str new-state)))))))

(define-syntax match/pos-any*
  (syntax-rules ()
    ((_ ((re func) more ...) state)
     (lambda (str)
       (define (consume recursed consumed-str consumed-state)
         (let-values (((new-str new-state)
                       (match/pos-any ((re func) more ...) consumed-state consumed-str)))
           (cond ((or
                   (eq? new-str #t)
                   (and (string? new-str)
                        (string=? new-str consumed-str)))
                  (error "error: match/pos-any* functions must return a modified string"))
                 ((eq? new-str #f)
                  (if recursed
                      (values consumed-str consumed-state)
                      (values #f #f)))
                 (else (consume #t new-str new-state)))))
       (consume #f str state)))))

; Tag matching
(define quoted-string "\"([^\"\\\\]*(\\\\.[^\"\\\\]*)*)\"")
(define reminder-tags
  (match/pos-any*
   ((#px",?\\s+#(\\w+)$"
     (lambda (match str state)
       (display (format "~a ~a ~a" match str state))(newline)
       (values (splice str (cadr match))
               (cons (capture str (cadr match)) state))))
    ((pregexp (string-append ",?\\s+#" quoted-string "$"))
     (lambda (match str state)
       (display (format "~a ~a ~a" match str state))(newline)
       (values (splice str (cadr match))
               (cons (capture str (cadr match)) state)))))
   '()))

(define remind-rule
  (match-every
   ((#px"^(?i:Remind\\s+)(.*)$"
     (lambda (match str state)
       (let-values (((rest targets)
                     (match-any*
                      ((#px"^(me|us),?\\s+(.*)$"
                        (lambda (match str state)
                          (display (format "cadr match: ~a" (cadr match)))(newline)
                          (display (format "caddr match: ~a" (caddr match)))(newline)
                          (values (caddr match)
                                  (cons (cadr match) state)))))
                      '()
                      (cadr match))))
         (values rest (cons 'targets targets)))))
    (#px"^(?:to|about|that)\\s+(.*)$"
     (lambda (match str state)
       (values (cadr match) state)))
    (#px"^(.*)$"
     (lambda (match str state)
       (values #t (cons (cons 'reminder (car match)) state)))))
   '()))




(define qs-re (pregexp quoted-string))
(regexp-match* qs-re "unquoted")
(regexp-match* qs-re "\"quoted string\"")
(regexp-match* qs-re "\"quoted \\\"embedded\\\" string\"")

(define tag (string-append "#" "(\\w+" "|" quoted-string ")"))
(define tag-re (pregexp tag))
(regexp-match* tag-re "#tag")
(regexp-match* tag-re "#\"quoted tag\"")
(regexp-match* tag-re "text")

