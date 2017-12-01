;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname find-root) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define ☠ 0.000000000000000000001) ; wat?

;; find-root : [Number -> Number] Number Number -> Number
;; ASSUME: f is continuous
;; (positive? (f a)) => (negative? (f b))
;; (negative? (f a)) => (positive? (f b))
;; a <= b
(define (find-root f a b)
  (cond [(< (- b a) ☠) (midpoint a b)]
        [else
         (local [(define f@a (f a))
                 (define f@b (f b))
                 (define m (midpoint a b))
                 (define f@m (f m))]
           (cond [(zero? f@a) a]
                 [(zero? f@b) b]
                 [(sign-change? f@a f@m)
                  (find-root f a m)]
                 [else
                  (find-root f m b)]))]))
         
(define (midpoint a b)
  (/ (+ a b) 2))

(define (sign-change? x y)
  (or (and (positive? x) (negative? y))
      (and (negative? x) (positive? y))))

(find-root sin -3 3)
(find-root cos 1 3)
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 









