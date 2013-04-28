#lang racket/base
(require 
 racket/list
 (planet dvanhorn/packrat:2:3)
 "common.rkt")

(provide
 tag-identifier?
 <tag>
 <tag-list>)

(define (tag-identifier? c)
  (char=? #\# c))

(define <tag>
  (parse <tag>
         (<tag>
          ((x := (? tag-identifier?) t := <tag-text+>) t))
         (<tag-text+>
          ((t := <tag-char> t* := <tag-text*>) (string-append (string t) t*)))
         (<tag-text*>
          ((t := <tag-text+>) t)
          (() ""))
         (<tag-char>
          ((c := (? (lambda (x) (not (or (char-whitespace? x)
                                         (char=? x #\,)))))) c))))

; A tag list is a tag followed by an optional comma then a space
; and then more tags
(define <tag-list>
  (parse <tag-list>
         (<tag-list>
          ((t := <tag> t* := <tag-list*>) (cons t t*)))
         (<tag-list*>
          (('#\, <whitespace+> t := <tag-list>) t)
          ((<whitespace+> t := <tag-list>) t)
          (() empty))))

(module+ test
  (require
   test-engine/racket-tests
   "util.rkt")
  
  (check-expect (parse-all <tag> "#tag") "tag")
  (check-error (parse-all <tag> "not a tag") "unparsed: \"not a tag\"")
  
  (check-expect (parse-all <tag-list> "#this, #that #theother") '("this" "that" "theother"))
  (check-error (parse-all <tag-list> "#this #that and #the #other") "unparsed: \" and #the #other\"")
  
  (test))
