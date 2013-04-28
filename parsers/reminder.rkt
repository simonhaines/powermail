#lang racket/base

(require
 (for-syntax racket/base)
 racket/string
 (planet dvanhorn/packrat)
 "datetime.rkt"
 "tag.rkt"
 "common.rkt")
(provide
 <reminder>
 event)

(define <subjects>
  (parse <subjects>
         (<subjects>
          (('#\m '#\e) '(self)))))

; Trim spaces start and end and title case first character
(define (prepare-content content)
  (let ([content-list (string->list (string-trim content))])
    (list->string (cons (char-titlecase (car content-list))
                        (cdr content-list)))))

(struct event (recipients content time tags)
  #:transparent)

(define <reminder>
  (parse <reminder>
         (<reminder>
          (('#\r '#\e '#\m '#\i '#\n '#\d <whitespace+> r := <recipients+> c := <content-spec> <whitespace*> t := <tag-list*>)
           (event r (prepare-content (cadr c)) (car c) t)))
         (<recipients+>
          ((s := <subjects> <whitespace+>) s))
         (<content-spec>
          ((<prefix> c := <content/time>) c)
          ((c := <content/time>) c))
         (<tag-list*>
          ((t := <tag-list>) t)
          (() '()))
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
                                         (tag-identifier? c)))))) (string c)))
         (<content-char+>
          ((c := (? (lambda (c) (not (char-whitespace? c)))) c* := <content-char*>) (string-append (string c) c*)))
         (<content-char*>
          ((c := <content-char+>) c)
          (() ""))))


(module+ test
  (require
   "locale.rkt"
   (planet bzlib/date-tz/plt)
   test-engine/racket-tests
   "util.rkt")
  
  (define-syntax (check-date-ref stx)
    (syntax-case stx ()
      ((_ (date* ...) parser str (recipient ...) content (expected-date* ...) (tag ...))
       #'(check-expect
          (parameterize ((*date-ref* (build-date/tz date* ... #:tz (*tz*))))
            (parse-all parser str))
          (event '(recipient ...) content (build-date/tz expected-date* ... #:tz (*tz*)) '(tag ...))))))

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
  (check-date-ref (2013 4 28) <reminder> "remind me that the 'end of days' is in december tomorrow"
                  (self) "You think the 'end of days' is in december" (2013 4 29 9) ())
  (check-date-ref (2013 4 28 12 55) <reminder> "remind me to ensure PLT dates in 10 minutes instead of SRFI dates"
                  (self) "Ensure PLT dates instead of SRFI dates" (2013 4 28 13 4) ())
  
  (test))

