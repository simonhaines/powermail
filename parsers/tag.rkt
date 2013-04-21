#lang racket/base
(require 
 racket/list
 (planet dvanhorn/packrat:2:3)
 "common.rkt")

(provide
 <tag>
 <tag-list>)

; A tag is '#' followed by any non-whitespace character or quoted string
; TODO quoted strings
(define <tag>
  (parse <tag>
         (<tag>
          (('#\# t := <tag-text>) t))
         (<tag-text>
          ((t := <tag-char> t* := <tag-text*>) (string-append (string t) t*)))
         (<tag-text*>
          ((t := <tag-char> t* := <tag-text*>) (string-append (string t) t*))
          (() ""))
         (<tag-char>
          ((c := (? (lambda (x)
                      (and
                       (not (char-blank? x))
                       (not (char=? x #\,)))))) c))))

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