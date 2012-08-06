#lang racket
(require (planet dvanhorn/packrat:2:3)
         "util.rkt")
(provide <addr-spec>)

; RFC2822 s3.4.1
(define <addr-spec>
  (parse <addr-spec>
         (<addr-spec>
          ((l := <local-part> '#\@ d := <domain>) (string-append l "@" d)))
         (<local-part>
          ((a := <dot-atom>) (list->string a)))
         (<domain>
          ((a := <dot-atom>) (list->string a)))
         (<dot-atom>
          ((a := <atom-text> a* := <atom-text>*) (cons a a*)))
         (<atom-text>*
          ((a := <atom-text> a* := <atom-text>*) (cons a a*))
          (() empty))
         (<atom-text>
          ((a := (? (lambda (c)
                      (or (char-alphabetic? c)
                          (char-numeric? c)
                          (char=? #\! #\# #\$ #\% #\& #\' #\* #\+ #\-
                                  #\/ #\= #\? #\^ #\_ #\` #\{ #\| #\} #\~))))) a))
         (<domain-text> ; Not used yet
          ((d := (? (lambda (c)
                      (or (char-in? c 33 90)
                          (char-in? c 94 126))))) d))))

; TODO quoted pairs for local-part and domain
;      address lists