#lang racket/base

(require
 (for-syntax racket/base)
 racket/string
 (planet dvanhorn/packrat)
 "datetime.rkt"
 "tag.rkt"
 "common.rkt"
 "util.rkt"
 "../reminder.rkt")

(provide
 <reminder>)

(define <subjects>
  (parse <subjects>
         (<subjects>
          (('#\m '#\e) '(self)))))

; Trim spaces start and end and title case first character
(define (format-content content)
  (let ([content-list (string->list (string-trim content))])
    (list->string (cons (char-titlecase (car content-list))
                        (cdr content-list)))))

(define <reminder>
  (parse <reminder>
         (<reminder>
          (('#\r '#\e '#\m '#\i '#\n '#\d <whitespace+> r := <recipients+> c := <content-spec> <whitespace*> t := <tag-list*>)
           (create-reminder r (format-content (cadr c)) (car c) t)))
         (<recipients+>
          ((s := <subjects> <whitespace+>) s))
         (<content-spec>
          ((<prefix> c := <content>) c)
          ((c := <content>) c))
         (<tag-list*>
          ((t := <tag-list>) t)
          (() '()))
         (<prefix>
          (('#\t '#\h '#\a '#\t <whitespace+>) #t)
          (('#\a '#\b '#\o '#\u '#\t <whitespace+>) #t)
          (('#\t '#\o <whitespace+>) #t))
         (<datetime>
          ((d := <datetime-spec> <word-boundary> <prefix>) d)
          ((d := <datetime-spec> <punctuation*> <word-boundary>) d))
         (<content>
          ((d := <datetime> c := <words>) (list d c)) ; TODO punctuation afer datetime
          ((c := <word> c+ := <content>) (list (car c+) (string-append c (cadr c+)))))
         (<word>
          (('#\m '#\y w := <word-boundary>) (string-append "your" w))
          (('#\m '#\e w := <word-boundary>) (string-append "you" w))
          (('#\i w := <word-boundary>) (string-append "you" w))
          ((c := <content-first-char> c* := <content-char*> w := <word-boundary>) (string-append c c* w)))
         (<words>
          ((w := <word> w* := <words>) (string-append w w*))
          (() ""))
         (<word-boundary>
          ((w := <whitespace+>) w)
          ((<terminal>) ""))
         (<punctuation*>
          ((p := (? char-punctuation?)) p)
          (() ""))
         (<content-first-char>
          ((c := (? (lambda (c) (not (or (char-whitespace? c)
                                         (terminal? c)
                                         (tag-identifier? c)))))) (string c)))  ; todo: (! (/ <whitespace> <terminal> <tag-identifier>))
         (<content-char+>
          ((c := (? (lambda (c) (not (or (char-whitespace? c)
                                         (terminal? c))))) c* := <content-char*>) (string-append (string c) c*)))
         (<content-char*>
          ((c := <content-char+>) c)
          (() ""))))


(module+ test
  (require
   (planet bzlib/date-tz/plt)
   "locale.rkt"
   "util.rkt"
   test-engine/racket-tests)
  
  (define-syntax (check-date-ref stx)
    (syntax-case stx ()
      ((_ (date* ...) parser str (recipient ...) content (expected-date* ...) (tag ...))
       #'(check-expect
          (parameterize ((*date-ref* (build-date/tz date* ... #:tz (*tz*))))
            (parse-all parser str))
          (list '(recipient ...) content (build-date/tz expected-date* ... #:tz (*tz*)) '(tag ...))))))
  
  (check-date-ref (2013 4 28 11 40) <reminder> "remind me in 20 minutes to take out the trash #todo #garbage"
                  (self) "Take out the trash" (2013 4 28 12) ("todo" "garbage"))
  (check-date-ref (2013 4 28 12 32) <reminder> "remind me tomorrow to get Mel to take out my trash for me"
                  (self) "Get Mel to take out your trash for you" (2013 4 29 9) ())
  (check-date-ref (2013 4 28 12 35) <reminder> "remind me thursday when i need reminding #reminder"
                  (self) "When you need reminding" (2013 5 2 9) ("reminder")) 
  (check-date-ref (2013 4 28 12 44) <reminder> "remind me at 5pm my cat's bowels need massaging #ick, #kill-the-cat"
                  (self) "Your cat's bowels need massaging" (2013 4 28 17) ("ick" "kill-the-cat"))
  (check-date-ref (2013 4 28) <reminder> "remind me that i think i need to read 'watership down' tomorrow"
                  (self) "You think you need to read 'watership down'" (2013 4 29 9) ())
  (check-date-ref (2013 4 28) <reminder> "remind me that tomorrow i think the 'end of days' is in december"
                  (self) "You think the 'end of days' is in december" (2013 4 29 9) ())
  (check-date-ref (2013 4 28 12 44) <reminder> "remind me at 5pm myopia degrades intelligence"
                  (self) "Myopia degrades intelligence" (2013 4 28 17) ())
  (check-date-ref (2013 4 28 12 55) <reminder> "remind me to ensure PLT dates in 10 minutes instead of SRFI dates"
                  (self) "Ensure PLT dates instead of SRFI dates" (2013 4 28 13 5) ())
  (check-date-ref (2013 5 18 13 00) <reminder> "remind me today at 5pm to ensure dates work OK"
                  (self) "Ensure dates work OK" (2013 5 18 17 00) ())
  (check-date-ref (2013 5 18 18 00) <reminder> "remind me today at 5pm to ensure dates work OK with trailing punctuation."
                  (self) "Ensure dates work OK with trailing punctuation." (2013 5 18 17 00) ())
  (check-date-ref (2013 5 18 18 00) <reminder> "remind me to ensure parsing with punctuation after trailing times today at 5pm."
                  (self) "Ensure parsing with punctuation after trailing times" (2013 5 18 17 00) ())
  (test))

