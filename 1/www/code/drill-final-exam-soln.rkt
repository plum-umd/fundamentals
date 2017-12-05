;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname drill-final-exam-soln) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; factorial : Natural -> Natural
;; Compute n!
(check-expect (factorial 5) 120)
(define (factorial n)
  (fact/a n 1)
  ;; Original:
  #;
  (cond [(zero? n) 1]
        [else
         (* n (factorial (sub1 n)))]))

;; Accumulator: factorial of numbers seen so far.
(define (fact/a n a)
  (cond [(zero? n) a]
        [else
         (fact/a (sub1 n) (* n a))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; largest : [NEListof Real] -> Real
;; Finds largest element in given non-empty list
(check-expect (largest (list 7)) 7)
(check-expect (largest (list 2 9 7)) 9)
(define (largest ns)
  ;; First element is the largest seen so far
  (largest/a (rest ns) (first ns))
  ;; Original:
  #;
  (cond [(empty? (rest ns)) (first ns)]
        [else
         (max (first xs)
              (largest (rest xs)))]))

;; largest/a : [Listof Real] Real -> Real
;; Find the largest number among elements in given list and n
(define (largest/a ns n)
  (cond [(empty? ns) n]
        [(cons? ns)
         (largest/a (rest ns) (max n (first ns)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; prod : [Listof Number] -> Number
;; Compute the product of the list
(check-expect (prod '()) 1)
(check-expect (prod (list 1 2 3 4 5)) 120)
(define (prod xs)
  ;; Using accumulator abstraction:
  (abs/a * 1 xs)
  ;; Using accumulator:
  #;
  (prod/a xs 1)
  ;; Original:
  #;
  (cond [(empty? xs) 1]
        [(cons? xs)
         (* (first xs)
            (prod (rest xs)))]))

;; prod/a : [Listof Number] Number -> Number
(define (prod/a xs a)
  (cond [(empty? xs) a]
        [(cons? xs)
         (sum/a (rest xs) (* (first xs) a))]))

;; sum : [Listof Number] -> Number
;; Compute the sum of the list
(check-expect (sum '()) 0)
(check-expect (sum (list 1 2 3 4 5)) 15)
(define (sum xs)
  ;; Using accumulator abstraction:
  (abs/a + 0 xs)
  ;; Using accumulator:
  #;
  (sum/a xs 0)
  ;; Original:
  #; 
  (cond [(empty? xs) 0]
        [(cons? xs)
         (+ (first xs)
            (sum (rest xs)))]))

;; sum/a : [Listof Number] Number -> Number
(define (sum/a xs a)
  (cond [(empty? xs) a]
        [(cons? xs)
         (sum/a (rest xs) (+ (first xs) a))]))

;; Abstraction of prod/a, suma:

;; abs/a : [X Y] [X Y -> Y] Y [Listof X] -> Y
(define (abs/a f a xs)
  (cond [(empty? xs) a]
        [(cons? xs)
         (abs/a f (f (first xs) a) (rest xs))]))

;; What does this remind you of?

;; It has the same signature as foldr!  In fact this is foldl.
;; While foldr is the fundamental abstraction of structural recursion on lists,
;; this is the fundamental abstraction of structural recursion with an
;; accumulator on lists.

(check-expect (largest.1 (list 7)) 7)
(check-expect (largest.1 (list 2 9 7)) 9)
(define (largest.1 xs)
  (abs/a max (first xs) (rest xs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; minf : [X] [X -> Real] [NEListof X] -> X
;; Find the earliest element that minimizes f.
(check-expect (minf sqr (list -4 2 8)) 2)
(check-expect (minf sqr (list -4 -2 8 2)) -2)
(define (minf f xs)
  ;; Initialize xn to first element, which is guaranteed to exist and
  ;; is the minimizing element seen so far.
  (minf/a f (rest xs) (first xs) (f (first xs)))

  ;; Original
  #;
  (cond [(empty? (rest xs)) (first xs)]
        [else
         (local [(define x1 (first xs))
                 (define y1 (f x1))
                 (define xn (minf f (rest xs)))
                 (define yn (f xn))]
           (if (<= y1 yn) x1 xn))]))

;; minf/a : [X] [X -> Real] [Listof X] X Real -> X
;; Accumulator: yn = (f xn) where xn is the earliest element
;; seen so far that minimizes f.
(define (minf/a f xs xn yn)
  (cond [(empty? xs) xn]
        [else
         (local [(define x1 (first xs))
                 (define y1 (f x1))]
           (if (<= yn y1)
               (minf/a f (rest xs) xn yn)
               (minf/a f (rest xs) x1 y1)))]))
        