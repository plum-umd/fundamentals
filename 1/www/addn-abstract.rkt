;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname addn-stub) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; add3-all : LoN -> LoN
;; Add 3 to each element of the list
(check-expect (add3-all '()) '())
(check-expect (add3-all (list 1 2 3)) (list 4 5 6))
(define (add3-all lon)
  (addN-all 3 lon))

;; add5-all : LoN -> LoN
;; Add 5 to each element of the list
(check-expect (add5-all '()) '())
(check-expect (add5-all (list 1 2 3)) (list 6 7 8))
(define (add5-all lon)
  (addN-all 5 lon))

;; addN-all : Number LoN -> LoN
;; Add n to each element of the list
(define (addN-all n lon)
  (cond [(empty? lon) '()]
        [(cons? lon)
         (cons (+ n (first lon))
               (addN-all n (rest lon)))]))
