;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname prod-sum-abstract) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; prod : LoN -> Number
;; Compute the product of all the numbers in the list
(check-expect (prod (list 1 2 3 4 5)) (* 1 2 3 4 5))
(define (prod lon)
  (sp * 1 lon))

;; sum : LoN -> Number
;; Compute the sum of all the numbers in the list
(check-expect (sum (list 1 2 3 4 5)) (+ 1 2 3 4 5))
(define (sum lon)
  (sp + 0 lon))

;; count : LoN -> Number
;; Count the number of numbers in the given list
(check-expect (count (list 1 2 3 4)) 4)
(define (count lon)
  (local [(define BASE 0)
          (define (mateo f c)
            (add1 c))]
    (foldr mateo BASE lon)))

(define (lon-template lon)
  (cond [(empty? lon) ...]
        [(cons? lon)
         (... (first lon)
              (lon-template (rest lon))
              ...)]))

;; sp : (Number Number -> Number) Number LoN -> Number
(define (sp op b lon)
  (cond [(empty? lon) b]
        [(cons? lon)
         (op (first lon)
             (sp op b (rest lon)))]))




