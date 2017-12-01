;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 5-7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Using the Design Recipe to help you program
;; 1. Problem Analysis & Data Definition
;; 2. Contract, Purpose, Signature
;; 3. Examples
;; 4. Function template
;; 5. Function definition
;; 6. Tests

;; The structure of your program follows the structure of your data.

;; An example of a recursive union

;;     +----------------------------------------+
;;     V                                        |
;; An [Listof X] is one of:                     |
;; - empty                                      |
;; - (cons X [Listof X])                        | 
;;                   +--------------------------+

;; A LoS is a [Listof CCIS-Username].
;; A CCIS-Username is a non-empty String.

;; count-students : LoS -> Number
;; Count the number of students in the given list.

(check-expect (count-students empty) 0)
(check-expect (count-students (cons "dvanhorn" empty)) 1)
(check-expect (count-students (list "dvanhorn" "shivers" "vkp" "rms")) 4)

(define (count-students los)
  (count (λ (x) true) los))

;; count : [X -> Boolean] [Listof X] -> Number
;; Count the number of elements for which the predicate holds
(define (count pred los)
  (cond [(empty? los) 0]
        [(cons? los)
         (cond [(pred (first los))
                (add1 (count pred (rest los)))]
               [else
                (count pred (rest los))])]))
