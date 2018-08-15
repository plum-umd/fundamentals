#lang class/0

;; A [Listof X] implements:
;; - length : -> Number
;;   Compute the length of this list of strings
;; - map [Y] : [X -> Y] -> [Listof Y]
;;   Apply the given function to each element of this list of strings
;; - foldr [Y] : [X Y -> Y] Y -> Y
;;   Fold the given function over this list


;; A [Listof Number] implements:
;; - sum : -> Number
;;   Sum all the elements of this list of numbers

;; sum : [Listof Number] -> Number



;; A (new empty%) implements [Listof X] and [Listof Number]
(define-class empty%
  ;; length : -> Number
  ;; Compute the length of this empty list
  (check-expect (send (new empty%) length) 0)
  (define (length) 0)

  ;; map [X] : [X -> X] -> [Listof X]
  ;; Apply the given function to each element of this empty list
  (check-expect (send (new empty%) map double) (new empty%))
  (define (map f)
    this)

  ;; foldr [Y] : [X Y -> Y] Y -> Y
  (check-expect (send (new empty%) foldr + 0) 0)
  (check-expect (send (new empty%) foldr string-append "") "")
  (define (foldr f b)
    b)

  ;; sum : -> Number
  ;; Sum this empty list of numbers
  (check-expect (send (new empty%) sum) 0)
  (define (sum)
    0))
  

;; A (new cons% X [Listof X]) implements [Listof X]
;; A (new cons% Number [Listof Number]) implements [Listof Number]
(define-class cons%
  (fields first rest)

  ;; cons-template : -> ???
  (define (cons-template)
    (... (send this first) ;; X
         (send (send this rest) cons-template) ;; ???
         ...))

  ;; length : -> Number
  ;; Compute the length of this non-empty list
  (check-expect
   (send (new cons% "3" (new cons% "4" (new empty%))) length)
   2)
  (define (length)
    (add1 (send (send this rest) length)))
    
  
  ;; map : [X -> X] -> [Listof X]
  ;; Apply the given function to each element of this non-empty list
  (check-expect
   (send (new cons% "3" (new cons% "4" (new empty%))) map double)
   (new cons% "33" (new cons% "44" (new empty%))))
  (define (map f)
    (new cons%
         (f (send this first))
         (send (send this rest) map f)))

  ;; foldr [Y] : [X Y -> Y] Y -> Y
  (check-expect (send (new cons% 5 (new cons% 3 (new empty%))) foldr + 0) 8)
  (check-expect
   (send (new cons% "5" (new cons% "3" (new empty%))) foldr string-append "")
   "53")
  (check-expect
   (send (new cons% "5" (new cons% "3" (new empty%))) foldr string-append "!")
   "53!")
  (define (foldr f b)
    (f (send this first)
       (send (send this rest) foldr f b)))


  ;; sum : -> Number
  ;; Sum this non-empty list of numbers
  (check-expect (send (new cons% 5 (new cons% 3 (new empty%))) sum) 8)
  (define (sum)
    (+ (send this first)
       (send (send this rest) sum))))
  
  


(define (double s)
  (string-append s s))