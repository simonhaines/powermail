#lang racket/base

(require
 (for-syntax racket/base)
 (planet dvanhorn/packrat)
 "datetime.rkt"
 "common.rkt")

(define <subjects>
  (parse <subjects>
         (<subjects>
          (('#\m '#\e) 'self))))

(define <reminder>
  (parse <reminder>
         (<reminder>
          (('#\r '#\e '#\m '#\i '#\n '#\d <whitespace+> r := <recipients+> c := <content-spec>) (cons r c)))
         (<recipients+>
          ((s := <subjects> <whitespace+>) s))
         (<content-spec>
          ((<prefix> c := <content/time>) c)
          ((c := <content/time>) c))
         (<prefix>
          (('#\t '#\h '#\a '#\t <whitespace+>) #t)
          (('#\a '#\b '#\o '#\u '#\t <whitespace+>) #t)
          (('#\t '#\o <whitespace+>) #t))
         (<content/time>
          ((d := <datetime-spec> <whitespace+> <prefix> c := <content+>) (list d c))
          ((d := <datetime-spec> <whitespace+> c := <content+>) (list d c))
          ((c := <content+> w := <whitespace+> ct := <content/time>) (string-append c w ct)))
         (<content+>
          ((c := <content-word> c* := <content*>) (string-append c c*)))
         (<content*>
          ((c := <content+>) c)
          (() ""))
         (<content-word>
          (('#\m '#\y w := <whitespace*>) (string-append "your" w))
          (('#\m '#\e w := <whitespace*>) (string-append "you" w))
          (('#\i w := <whitespace*>) (string-append "you" w))
          ((c := <content-first-char> c* := <content-char*> w := <whitespace*>) (string-append c c* w)))
         
         ; TODO use (char-general-category char) to create char sets
         ; see: http://www.fileformat.info/info/unicode/category/index.htm
         (<content-first-char>
          ((c := (? (lambda (c) (not (or (char-whitespace? c)
                                         (char=? #\# c)))))) (string c)))
         (<content-char+>
          ((c := (? (lambda (c) (not (char-whitespace? c)))) c* := <content-char*>) (string-append (string c) c*)))
         (<content-char*>
          ((c := <content-char+>) c)
          (() ""))))


(module+ test
  (require
   test-engine/racket-tests
   "util.rkt")
  
  (check-expect (parse-all <reminder> "remind me in 20 minutes to take out the trash (out the back)") '(self "take out the trash (out the back)"))
  (check-expect (parse-all <reminder> "remind me tomorrow to take out my trash for me") '(self "take out your trash for you"))
  (check-expect (parse-all <reminder> "remind me thursday when i need reminding") '(self "when you need reminding"))
  (check-expect (parse-all <reminder> "remind me to take out my trash") '(self "take out your trash"))
  (check-expect (parse-all <reminder> "remind me my cat's bowels need massaging") '(self "your cat's bowels need massaging"))
  (check-expect (parse-all <reminder> "remind me that i think i read the 'end of days' is in december") '(self "you think you read the 'end of days' is in december"))
  
  (test))

