#lang racket/base

(require
 (for-syntax racket/base)
 (planet dvanhorn/packrat)
 "common.rkt")

(define <recipients>
  (parse <recipients>
         (<recipients>
          (('#\m '#\e) 'self))))

(define <reminder>
  (parse <reminder>
         (<reminder>
          (('#\r '#\e '#\m '#\i '#\n '#\d <whitespace+> r := <recipients> <whitespace+> c := <content>) (list r c)))
         (<content>
          (('#\t '#\h '#\a '#\t <whitespace+> c := <raw-content>) c)
          (('#\a '#\b '#\o '#\u '#\t <whitespace+> c := <raw-content>) c)
          (('#\t '#\o <whitespace+> c := <raw-content>) c)
          ((c := <raw-content>) c))
         (<raw-content>  ; TODO don't replace words in quote phrases
          (('#\m '#\y w := <space+> c := <raw-content*>) (string-append "your" w c))
          (('#\m '#\e w := <space+> c := <raw-content*>) (string-append "you" w c))
          (('#\i w := <space+> c := <raw-content*>) (string-append "you" w c))
          ((c := <content-word> w := <space+> r := <raw-content>) (string-append c w r))
          ((c := <content-word*>) c))
         (<raw-content*>
          ((c := <raw-content>) c)
          (() ""))

         (<content-word>
          ((c := <content-char> c* := <content-char*>) (string-append c c*)))
         (<content-word*>
          ((c := <content-word>) c)
          (() ""))
         ; TODO use (char-general-category char) to create char sets
         (<content-char>
          ((c := (? char-alphabetic?)) (string c))
          (('#\') "'"))
         (<content-char*>
          ((c := <content-char> c* := <content-char*>) (string-append c c*))
          (() ""))
         (<space+>
          ((s := (? (lambda (x) (not (char-alphabetic? x)))) s* := <space*>) (string-append (string s) s*)))
         (<space*>
          ((s := <space+>) s)
          (() ""))))


(module+ test
  (require test-engine/racket-tests)
  
  (define (parse-string parser str)
    (parse-result-semantic-value (parser (packrat-string-results "<str>" str))))
  
  (check-expect (parse-string <reminder> "remind me to take out the trash") '(self "take out the trash"))
  (check-expect (parse-string <reminder> "remind me when i need reminding") '(self "when you need reminding"))
  (check-expect (parse-string <reminder> "remind me to take out my trash") '(self "take out your trash"))
  (check-expect (parse-string <reminder> "remind me my cat's bowels need massaging") '(self "your cat's bowels need massaging"))
  (check-expect (parse-string <reminder> "remind me that i think i read the 'end of days' is in december") '(self "you think you read the 'end of days' is in december"))
  
  (test))

