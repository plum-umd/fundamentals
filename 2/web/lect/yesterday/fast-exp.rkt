;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fast-exp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Compute n^m
;; Natural Natural -> Natural
(define (fast-exp n m)
  (cond [(zero? m) 1]
        [(even? m)
         (sqr (fast-exp n (half m)))]
        [else 
         (* n
            (fast-exp n (sub1 m)))]))

(define (half m)
  (quotient m 2))

(check-expect (fast-exp 5 0) 1)
(check-expect (fast-exp 3 4) 81)

(check-expect (fast-exp 5 100) 
              (sqr (fast-exp 5 50)))
