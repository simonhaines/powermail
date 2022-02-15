#lang racket/base
(require
 (planet williams/uuid/uuid))

(provide
 create-reminder
 (struct-out reminder))

(struct reminder (id recipients content time tags) #:transparent)

(define (create-reminder recipients content time tags)
  (reminder (uuid->hex-string (make-uuid-4)) recipients content time tags))