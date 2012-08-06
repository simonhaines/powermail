#lang racket
(require (planet dvanhorn/packrat:2:3))
(provide <tag>
         <tag-list>)

; A tag is '#' followed by any non-whitespace character or quoted string
; TODO quoted strings
(define <tag>
  (parse <tag>
         (<tag>
          (('#\# a := <tag-text>) (list->string a)))
         (<tag-text>
          ((a := <tag-char> b := <tag-text>*) (cons a b)))
         (<tag-text>*
          ((a := <tag-char> a* := <tag-text>*) (cons a a*))
          (() empty))
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
          ((t := <tag> t* := <tag-list>*) (cons t t*)))
         (<tag-list>*
          (('#\, <whitespace>+ t := <tag-list>) t)
          ((<whitespace>+ t := <tag-list>) t)
          (() empty))
         (<whitespace>+
          (('#\space <whitespace>*) 'space))
         (<whitespace>*
          (('#\space <whitespace>*) 'space)
          (() empty))))
