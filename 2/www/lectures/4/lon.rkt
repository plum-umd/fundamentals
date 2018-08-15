#lang class/0

;; A LoN implements:
;; - length : -> Number
;;   Compute the length of this list of numbers
;; - map : [Number -> Number] -> LoN
;;   Apply the given function to each element of this list of numbers

;; A (new empty-lon%) implements LoN
(define-class empty-lon%
  ;; length : -> Number
  ;; Compute the length of this empty list of numbers
  (check-expect (send (new empty-lon%) length) 0)
  (define (length) 0)

  ;; map : [Number -> Number] -> LoN
  ;; Apply the given function to each element of this empty list of numbers
  (check-expect (send (new empty-lon%) map add1) (new empty-lon%))
  (define (map f)
    this))

;; A (new cons-lon% Number LoN) implements LoN
(define-class cons-lon%
  (fields first rest)

  ;; cons-lon-template : -> ???
  (define (cons-lon-template)
    (... (send this first) ;; Number
         (send (send this rest) cons-lon-template) ;; ???
         ...))

  ;; length : -> Number
  ;; Compute the length of this non-empty list of numbers
  (check-expect
   (send (new cons-lon% 3 (new cons-lon% 4 (new empty-lon%))) length)
   2)
  (define (length)
    (add1 (send (send this rest) length)))
    
  
  ;; map : [Number -> Number] -> LoN
  ;; Apply the given function to each element of this non-empty list of numbers
  (check-expect
   (send (new cons-lon% 3 (new cons-lon% 4 (new empty-lon%))) map add1)
   (new cons-lon% 4 (new cons-lon% 5 (new empty-lon%))))
  (define (map f)
    (new cons-lon%
         (f (send this first))
         (send (send this rest) map f))))





  


