#lang class/0

;; A LoS implements:
;; - length : -> Number
;;   Compute the length of this list of strings
;; - map : [String -> String] -> LoS
;;   Apply the given function to each element of this list of strings

;; A (new empty-los%) implements LoS
(define-class empty-los%
  ;; length : -> Number
  ;; Compute the length of this empty list of strings
  (check-expect (send (new empty-los%) length) 0)
  (define (length) 0)

  ;; map : [String -> String] -> LoS
  ;; Apply the given function to each element of this empty list of strings
  (check-expect (send (new empty-los%) map double) (new empty-los%))
  (define (map f)
    this))

;; A (new cons-los% Number LoS) implements LoS
(define-class cons-los%
  (fields first rest)

  ;; cons-los-template : -> ???
  (define (cons-los-template)
    (... (send this first) ;; String
         (send (send this rest) cons-los-template) ;; ???
         ...))

  ;; length : -> Number
  ;; Compute the length of this non-empty list of strings
  (check-expect
   (send (new cons-los% "3" (new cons-los% "4" (new empty-los%))) length)
   2)
  (define (length)
    (add1 (send (send this rest) length)))
    
  
  ;; map : [String -> String] -> LoS
  ;; Apply the given function to each element of this non-empty list of strings
  (check-expect
   (send (new cons-los% "3" (new cons-los% "4" (new empty-los%))) map double)
   (new cons-los% "33" (new cons-los% "44" (new empty-los%))))
  (define (map f)
    (new cons-los%
         (f (send this first))
         (send (send this rest) map f))))


(define (double s)
  (string-append s s))