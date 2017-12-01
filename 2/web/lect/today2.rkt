#lang class/1

;; A SLoN is a list of numbers sorted in ascending order
;; A SLoN implements
;; - contains? : Number -> Boolean
;;   Does this list contain the number?
;; - insert : Number -> SLoN
;;   Insert the given number into the sorted list
;; - sorted? : -> Boolean
;;   Is this sorted list sorted?
;; - smallest : Number -> Number
;;   Picks the smallest number between the given number and elements of this list.

;; A NESLoN implements
;; - minimum : -> Number
;;   Produce minimum element of this non-empty list.
;; and SLoN.

;; ∀ LoN ls,
;; ∀ Number n,
;; ((ls . insert n) . contains? n)

;; ((ls . insert n) . sorted?)

(define-class mt%
  (define (sorted?) true)
  (define (smallest n) n)
  (define (insert n)
    (new cons% n this)))

(define-class cons%
  (fields first rest)
  
  (define (sorted?)  
    (and (= (this . first)
            (this . minimum))
         (this . rest . sorted?)))
  
  (define (minimum)
    (this . smallest (this . first)))
  
  (define (smallest n)        
    (this . rest . smallest (min (this . first) n)))
  
  (define (insert n)
    (cond [(< n (this . first))
           (new cons% n this)]
          [else
           (new cons% (this . first)
                (this . rest . insert n))])))
  
    

(check-expect ((new mt%) . sorted?) true)
(check-expect ((new cons% 5 (new mt%)) . sorted?) true)
(check-expect ((new cons% 5 (new mt%)) . sorted?) true)
(check-expect ((new cons% 5 (new cons% 3 (new mt%))) . sorted?) false)

;; Counter example to property:
(((new cons% 5 (new cons% 2 (new mt%))) . insert 9) . sorted?)
