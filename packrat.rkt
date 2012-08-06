#lang racket
(require (planet dvanhorn/packrat:2:3)
         "parsers/email.rkt"
         "parsers/tag.rkt"
         "parsers/time.rkt")

#|
Waite, Goos, "Compiler Construction", Appendix A, p. 383, gives some EBNF
to BNF translation rules. With a somewhat different notation:
1. a (b) c := a x c, x: b.
2. a[b]c := a c | a(b)c.
3. a u+ c := a x c, x: u | x u.
4. a u* c := a[u+]c.
5. a || t := a(t a)*.
where a, b, c are arbitrary RHS rules, x a unique non-terminal, u a single
or parenthesized grammar symbol, and t a terminal.

See: packrat-test.rkt
|#

; This generator collapses whitespace to a single '#\space token
; and converts all other tokens to lower case
(define (generator str)
  (base-generator->results
   (let ((idx (box 0))
         (len (string-length str))
         (pos (box (top-parse-position "<string>"))))
     (lambda ()
       (let ((current-idx (unbox idx))
             (current-pos (unbox pos)))
         (if (= current-idx len)
             (values current-pos #f)
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
                           (update-parse-position next-pos (string-ref str next-idx))))))))))))

; Generator
(define (parse-str parser str)
  (let* ((result (generator str))
         (parse-result (parser result)))
    (if (parse-result-successful? parse-result)
        (parse-result-semantic-value parse-result)
        (parse-result-error parse-result))))

(define <reminder>
  (parse <reminder>
         (<reminder>
          ((<prefix> s := <term>+ t := <tags>*) (list (cons 'subject (string-join s " "))
                                                      (cons 'tags t))))
         (<prefix>
          (('#\r '#\e '#\m '#\i '#\n '#\d '#\space) 'remind))
         (<term>+
          ((t := <term> t* := <term>*) (cons t t*))
          ((t := <tag> '#\space t+ := <term>+) (cons (string-append "#" t) t+))
          ((t := <tag> '#\, '#\space t+ := <term>+) (cons (string-append "#" t ",") t+)))
         (<term>*
          (('#\space t := <term>+) t)
          (() empty))
         (<term>
          ((c := <initial-term-char> c* := <term-char>*) (list->string (cons c c*))))
         (<initial-term-char>
          ((c := (? (lambda (c)
                      (not (or (char-blank? c)
                               (char=? c #\#)))))) c))
         (<term-char>*
          ((c := <term-char> c* := <term-char>*) (cons c c*))
          (() empty))
         (<term-char>
          ((c := (? (lambda (c)
                      (not (char-blank? c))))) c))
         (<tags>*
          (('#\space t := <tag-list>) t)
          (() empty))))

; This test picks out phrases and lets the rest pass through
(define <test>
  (parse <test>
         (<test>
          (('#\t '#\1 t := <test>) (cons 'test1 t))
          (('#\t t := <test>) (cons 'test2 t))
          ((c := (? (lambda (x) #t)) t := <test>) (cons c t))
          (() empty))))
