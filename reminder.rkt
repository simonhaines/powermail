#lang racket/base
(require
 (planet williams/uuid/uuid))

(provide
 create
 (struct-out reminder))

(struct reminder (id recipients content time tags) #:transparent)

(define (create recipients content time tags)
  (reminder (uuid->hex-string (make-uuid-4)) recipients content time tags))