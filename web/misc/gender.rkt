#lang class/1
(provide m f)
(define-class male%   ; implements gender
  (define (is-male?) true)
  (define (is-female?) false))
(define-class female% ; implements gender
  (define (is-male?) false)
  (define (is-female?) true))

(define m (new male%))
(define f (new female%))
