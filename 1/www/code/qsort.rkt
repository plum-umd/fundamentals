;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname qsort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; qsort : [Listof Number] -> [Listof Number]
;; Sort the list in ascending order
(check-expect (qsort (list 11 8 14 7)) (list 7 8 11 14))
(check-expect (qsort (list 1 1 1 1)) (list 1 1 1 1))
(define (qsort lon)
  (cond [(empty? lon) '()]
        [(cons? lon)
         (local [(define pivot (first lon))]
           (append (qsort (smaller (rest lon) pivot))
                   (list pivot)
                   (qsort (bigger (rest lon) pivot))))]))

;; smaller : [Listof Number] Number -> [Listof Number]
;; Produce a list of elements smaller than the given number
(check-expect (smaller (list 1 2 3) 2) (list 1))
(define (smaller lon n)
  (filter (λ (fred) (< fred n)) lon))

;; bigger : [Listof Number] Number -> [Listof Number]
;; Produce a list of elements bigger than or equal to the given number
(check-expect (bigger (list 1 2 3) 2) (list 2 3))
(define (bigger lon n)
  (filter (λ (barney) (>= barney n)) lon))
