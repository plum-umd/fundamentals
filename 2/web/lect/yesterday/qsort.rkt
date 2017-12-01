;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname qsort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A LoN is one of:
;; - empty
;; - (cons Number LoN)

;; Sort list of numbers in ascending order
;; LoN -> LoN
(define (isort lon)
  (cond [(empty? lon) empty]
        [(cons? lon) 
         (insert (first lon)
                 (isort (rest lon)))]))

;; Insert given number into right spot in sorted list.
;; Number LoN -> LoN
(define (insert n slon)
  (cond [(empty? slon) (list n)]
        [(cons? slon)
         (cond [(> n (first slon))
                (cons (first slon) (insert n (rest slon)))]
               [else
                (cons n slon)])]))

(check-expect (isort empty) empty)
(check-expect (isort (list 3 2 1 4)) (list 1 2 3 4))









