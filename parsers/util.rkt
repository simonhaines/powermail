#lang racket
(require
 srfi/1
 (planet "dvanhorn/packrat/combinator.ss")
 (planet "dvanhorn/packrat/parser-struct.rkt"))
(provide char-range->list
         char-in?
         chars->num
         pair->parser
         pair-list->parser
         string-list->parser)

(define (char-range->list low hi)
  (map integer->char (stream->list (in-range low hi))))

(define (char-in? c low hi)
  (let ((i (char->integer c)))
    (and (>= i low)
         (<= i hi))))

(define (chars->num . chars)
  (string->number (list->string chars)))

; Takes a (string . value) pair and generates a parser that attempts to
; parse the string, returning the value if successful.
(define (pair->parser pair)
  (if (or (not (pair? pair))
          (null? pair)
          (not (string? (car pair))))
      (error "argument must be a (string . value) pair")
      (parser (lambda (results)
                (results->result results (gensym)
                                 (lambda ()
                                   ((fold-right
                                     (lambda (chr next)
                                       (parser (lambda (results)
                                                 ((packrat-check-base chr (lambda (_) next))
                                                  results))))
                                     (parser (lambda (results)
                                               (make-result (cdr pair) results)))
                                     (string->list (car pair)))
                                    results)))))))

; Takes a list of (string . value) pairs and generates a parser that attempts
; to alternately parse the string in each pair, returning the value of the
; first successful parse.
(define (pair-list->parser pair-list)
  (if (or (not (pair? pair-list))
          (null? pair-list)
          (not (pair? (car pair-list))))
      (error "argument must be a list of (string . value) pairs")
      (parser (lambda (results)
                (results->result results (gensym)
                                 (lambda ()
                                   ((fold
                                     (lambda (pair next)
                                       (if (null? next)
                                           (pair->parser pair)
                                           (parser (lambda (results)
                                                     ((packrat-or next (pair->parser pair))
                                                      results)))))
                                     empty
                                     pair-list) results)))))))

; Takes a list of strings and builds a parser that attempts to parse each
; string and return the string if successfully parsed
(define (string-list->parser str-list)
  (pair-list->parser (map (lambda (str) (cons str str)) str-list)))