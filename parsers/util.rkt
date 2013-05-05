#lang racket/base
(require
 racket/stream
 racket/contract
 racket/list
 (planet dvanhorn/packrat)
 (planet "dvanhorn/packrat/parser-struct.rkt"))

(provide char-range->list
         char-in?
         chars->num
         pair->parser
         pair-list->parser
         string-list->parser
         parse-all
         terminal?)

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
                                   ((foldr
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
                                   ((foldl
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

; A generator that collapses whitespace to a single '#\space token
; and converts all other tokens to lower case retaining the character in the semantic value
(define (terminal? c)
  (char=? c #\034))

(define (string/downcase-generator str)
  (base-generator->results
   (let ((len (string-length str))
         (idx (box 0))
         (pos (box (top-parse-position "<string>")))
         (eof (box #f)))
     (lambda ()
       (let ((current-idx (unbox idx))
             (current-pos (unbox pos)))
         (cond [(unbox eof)
                (values current-pos #f)]
               [(= current-idx len)
                (set-box! eof #t)
                (values current-pos (cons #\034 #\034))] ; Terminal: 034 = ASCII 'file separator'
               [else 
                (let ((ch (string-ref str current-idx)))
                  (let skip ((next-idx (add1 current-idx))
                             (next-pos (update-parse-position current-pos ch)))
                    (if (or (= next-idx len)
                            (not (char-blank? ch))
                            (not (char-blank? (string-ref str next-idx))))
                        (begin
                          (set-box! idx next-idx)
                          (set-box! pos next-pos)
                          (values current-pos
                                  (cons (if (char-blank? ch) #\space
                                            (char-downcase ch)) ch)))
                        (skip (add1 next-idx)
                              (update-parse-position next-pos (string-ref str next-idx))))))]))))))

; TODO enclose in submodule
(define (parse-all parser str)
  (let* ([result (parser (string/downcase-generator str))]
         [amount (or (and (parse-result-successful? result)
                          (parse-position-column (parse-results-position (parse-result-next result))))
                     0)]
         [length (string-length str)])
    (when (not (= amount length))
      (error (format "unparsed: ~s" (substring str amount))))
    (parse-result-semantic-value result)))

(module test racket/base
  (provide test)
  (define test 'test))