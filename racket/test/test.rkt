#lang racket
(require
 srfi/1
 (planet dvanhorn/packrat)
 (planet "dvanhorn/packrat/combinator.ss")
 (planet "dvanhorn/packrat/parser-struct.rkt"))

#;(define <test>
    (parse <test>
           (<test>
            (('#\t) 'ok))))


#;(define test
    (parser 
     (lambda (results)
       (results->result results 'test
                        (lambda ()
                          ((packrat-check-base '#\t
                                               (lambda (dummy)
                                                 (parser (lambda (results)
                                                           (make-result 'ok results)))))
                           results))))))

#;(define test
    (parser 
     (lambda (results)
       (results->result results 'test
                        (lambda ()
                          ((packrat-check-base #\t
                                               (lambda (dummy)
                                                 (parser (lambda (results)
                                                           ((packrat-check-base #\e
                                                                                (lambda (dummy)
                                                                                  (parser (lambda (results)
                                                                                            (make-result 'ok results)))))
                                                            results)))))
                           results))))))

(define test
  (parser 
   (lambda (results)
     (results->result results 'test
                      (lambda ()
                        (
                         
                         (parser (lambda (results)
                                   ((packrat-check-base #\t
                                                        (lambda (dummy)
                                                          
                                                          (parser (lambda (results)
                                                                    ((packrat-check-base #\e
                                                                                         (lambda (dummy)
                                                                                           
                                                                                           (parser (lambda (results)
                                                                                                     (make-result 'ok results)))))
                                                                     results)))))
                                    results)))
                         
                         results))))))

(define t (packrat-string-results "fn" "test"))
(parse-result-semantic-value (test t))
(parse-results-base (parse-result-next (test t)))


(define (gen str)
  (fold-right
   (lambda (chr next)
     (parser (lambda (results)
               ((packrat-check-base chr
                                    (lambda (_) next))
                results))))
   (parser (lambda (results)
             (make-result `(ok . ,str) results)))
   (string->list str)))

(define (gen-test str)
  (parser (lambda (results)
            (results->result results 'gen-test
                             (lambda ()
                               ((gen str) results))))))

(define x (packrat-string-results "fn" "testes"))
(parse-result-semantic-value ((gen-test "test") x))


(define (gen-or str-list)
  (fold
   (lambda (str p)
     (if (null? p)
         (gen str)
         (parser (lambda (results)
                   ((packrat-or (gen str) p) results)))))
   empty
   str-list))

(define (gen-or-test str-list)
  (parser (lambda (results)
            (results->result results 'gen-or-test
                             (lambda ()
                               ((gen-or str-list) results))))))

(define ox (packrat-string-results "gen-or" "testes12"))
(parse-result-semantic-value ((gen-or-test '("test" "testes" "test234")) ox))
#;(parse-error-expected (parse-result-error ((gen-or-test '("test" "testes")) ox)))

; Takes a (string . value) pair and generates a parser that attempts to
; parse the string, returning the value if successful.
(define (pair->parser pair)
  (if (or (not (pair? pair))
          (null? pair)
          (not (string? (car pair))))
      (error "argument must be a (string . value) pair")
      (parser (lambda (results)
                (results->result results (gensym);'pair-parser
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
                (results->result results (gensym);'pair-list-parser
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

#;(define px (packrat-string-results "pp" "testables12"))
#;(parse-result-semantic-value ((pair->parser '("testa" . px)) px))
(define px2 (packrat-string-results "p2" "testable"))
(parse-result-semantic-value ((pair-list->parser '(("asdd" . fail) ("testab" . pass) ("xyz" . xyz))) px2))
#;(define px3 (packrat-string-results "p2" "test2"))
#;(parse-result-semantic-value ((string-list->parser '("test1" "test2" "test3")) px3))
