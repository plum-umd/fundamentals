;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname merge) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; merge : [Listof Number] [Listof Number] -> [Listof Number]
;; Merge two sorted lists into a sorted list.
;; Assume: inputs sorted (ascending)
(check-expect (merge '() '()) '())
(check-expect (merge '() (list 1 2 3))
              (list 1 2 3))
(check-expect (merge (list 1 2 3) '())
              (list 1 2 3))
(check-expect (merge (list 1 3 5) (list 2 4 6))
              (list 1 2 3 4 5 6))
(check-expect (merge (list 1 3 5) (list 2))
              (list 1 2 3 5))
(check-expect (merge (list 2 4 6) (list 1 3 5))
              (list 1 2 3 4 5 6))
(check-expect (merge (list 1 1 1 1) (list 1 1 1 1))
              (list 1 1 1 1 1 1 1 1))
(check-expect (merge (list 1 2 3) (list 4))
              (list 1 2 3 4))

;; (merge '() lon) = lon
;; (merge lon '()) = lon

;; (cons (first nelon) (rest nelon)) = nelon

(define (merge l1 l2)
  (cond [(and (empty? l1) (empty? l2)) '()]
        [(and (empty? l1) (cons? l2)) l2]
        [(and (cons? l1) (empty? l2)) l1]          
        [(and (cons? l1) (cons? l2))
         (if (< (first l1) (first l2))
             (cons (first l1) (merge (rest l1) l2))
             (cons (first l2) (merge l1 (rest l2))))]))
         


              
         
