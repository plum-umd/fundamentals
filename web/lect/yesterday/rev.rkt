;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rev) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; LoN -> LoN
;; Reverse given list of numbers
#|
(define (rev ls)
  (cond [(empty? ls) ls]
        [(cons? ls)
         (app (rev (rest ls))
              (list (first ls)))]))

(define (app ls1 ls2)
  (cond [(empty? ls1) ls2]
        [(cons? ls1)
         (cons (first ls1)
               (app (rest ls1) ls2))]))
|#

(define (rev ls)
  (rev/a ls empty))

;; Computes (append (rev ls) a)
(define (rev/a ls a)
  (cond [(empty? ls) a]
        [(cons? ls)
         (rev/a (rest ls) (cons (first ls) a))]))

(check-expect (rev/a (list 1 2 3) (list 6 5 4))
              (list 3 2 1 6 5 4))

(check-expect (rev empty) empty)
(check-expect (rev (list 2 3))
              (list 3 2))
(check-expect (rev (list 1 2 3))
              (list 3 2 1))



         