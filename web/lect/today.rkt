#lang class/1

;; A LoN implements
;; - contains? : Number -> Boolean
;;   Does this list contain the number?
;; - insert : Number -> LoN
;;   Insert the given number into the list

;; ∀ LoN ls,
;; ∀ Number n,
;; ((ls . insert n) . contains? n)

(define-class mt%  
  (define (contains? number)
    false)
  
  (define (insert number)
    (new cons% number this)))
  
(define-class cons%
  (fields first rest)
  (define (contains? number)
    (or (= (this . first) number)
        (this . rest . contains? number)))
  
  (define (insert number)
    (new cons% number this)))

(check-expect (((new mt%) . insert 5) . contains? 5) true)
(check-expect (((new cons% 6 (new mt%)) . insert 5) . contains? 5) true)

;; LoN Number -> Boolean
(define (insert-contains? ls n)
  ((ls . insert n) . contains? n))

;; LoN Number -> Boolean
(define (insert-contains-five? ls n)
  ((ls . insert n) . contains? 5))

(check-expect (insert-contains? (new mt%) 5) true)
(check-expect (insert-contains? (new cons% 6 (new mt%)) 5) true)

;; Natural -> List
;; Produce a list of length random elements.
(define (build-random-list length)
  (cond [(zero? length) (new mt%)]
        [else
         (new cons% 
              (random 4294967087)
              (build-random-list (sub1 length)))]))

(define (do i f)
  (cond [(zero? i) 'done]
        [else
         (local [(define r (f 'ignore-me))]
           (do (sub1 i) f))]))


         


(do 500
  (λ (_)
    (check-expect (insert-contains-five? (new cons% 5 (build-random-list 1000))
                                         (random 4294967087))
                  true)))
  

  