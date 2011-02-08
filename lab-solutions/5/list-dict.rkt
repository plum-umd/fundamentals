#lang class2

(provide empty-list-dict)

(require "dict.rkt")

; A [ListDict V] is one of:
;  - (ld-empty%)
;  - (ld-cons% Key V [ListDict V])

(define-class ld-empty%
  (implements dict<%>)

  (check-expect ((ld-empty%) . has-key? 1) false)

  (define/public (has-key? k)
    false)

  (define/public (lookup k)
    (error "Empty ListDict doesn't contain key" k))

  (check-expect ((ld-empty%) . set 1 "one" . lookup 1) "one")

  (define/public (set k v)
    (ld-cons% k v this))

  (define/public (size)
    0)

  (define/public (extend that)
    that)

  )

(define-class ld-cons%
  (implements dict<%>)
  (fields key value rest)

  (check-expect ((ld-cons% 1 "one" (ld-empty%)) . has-key? 1) true)
  (check-expect ((ld-cons% 1 "one" (ld-empty%)) . has-key? 2) false)

  (define/public (has-key? k)
    (if (equal? k (field key))
      true
      ((field rest) . has-key? k)))

  (check-expect ((ld-cons% 1 "one" (ld-empty%)) . lookup 1) "one")
  (check-error  ((ld-cons% 1 "one" (ld-empty%)) . lookup 2))

  (define/public (lookup k)
    (if (equal? k (field key))
      (field value)
      ((field rest) . lookup k)))

  (check-expect ((ld-empty%) . set 1 "one" . set 2 "two" . lookup 1) "one")
  (check-expect ((ld-empty%) . set 1 "one" . set 2 "two" . lookup 2) "two")
  (check-expect ((ld-empty%) . set 1 "one" . set 1 "uno" . lookup 1) "uno")
  (check-error  ((ld-empty%) . set 1 "one" . set 1 "uno" . lookup 3))

  (define/public (set k v)
    (ld-cons% k v this))

  (define/public (size)
    (+ 1 ((field rest) . size)))

  (define/public (extend that)
    ((field rest) . extend that . set (field key) (field value)))

  )

; empty-list-dict : [ListDict V]
(define empty-list-dict
  (ld-empty%))