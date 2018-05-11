#lang class/0
(provide (all-defined-out))

;; Empty list of numbers
(define-class empty%
  (define (length) 0)
  (define (map f) this)
  (define (foldr f b) b))

;; Non-empty lists of numbers
(define-class cons%  
  (fields first rest)
  (define (length) (add1 (send (send this rest) length)))
  (define (map f) (new cons% (f (send this first)) (send (send this rest) map f)))
  (define (foldr f b) (f (send this first) (send (send this rest) foldr f b))))