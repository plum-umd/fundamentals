#lang class/0
(provide (all-defined-out))

(define-class empty-los%
  (define (length) 0)
  (define (map f) this))

(define-class cons-los%  
  (fields first rest)
  (define (length) (add1 (send (send this rest) length)))
  (define (map f) (new cons-los% (f (send this first)) (send (send this rest) map f))))
