;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lon) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;    +---------------------------------+
;;    |                                 |
;;    v                                 |
;; A LoN (list of numbers) is one of:   |
;; - '()                                |
;; - (cons Number LoN)                  |
;;                 |                    |
;;                 +--------------------+

(define (lon-template lon)
  (cond [(empty? lon) ...]
        [(cons? lon)
         (... (first lon)
              ...
              (lon-template (rest lon))
              ...)]))

#|
Cons is just like (define-struct cons (first rest)) but
with shorter names:

cons   ~ make-cons
first  ~ cons-first
rest   ~ cons-rest
cons?  ~ cons?
|#

;; count : LoN -> Natural
;; Compute how many numbers there are in the list.
(check-expect (count '()) 0)
(check-expect (count (cons 8 (cons 3 (cons 1 '())))) 3)
(define (count lon)
  (cond [(empty? lon) 0]
        [(cons? lon)
         (add1 (count (rest lon)))]))

;; contains-five? : LoN -> Boolean
;; Are any of the numbers 5?
(check-expect (contains-five? '()) #false)
(check-expect (contains-five? (cons 3 (cons 4 '()))) #false)
(check-expect (contains-five? (cons 3 (cons 5 '()))) #true)
(define (contains-five? lon)
  (cond [(empty? lon) #false]
        [(cons? lon)
         (or (= (first lon) 5)
             (contains-five? (rest lon)))]))

;; sum : LoN -> Number
;; Compute the sum of all given numbers.
(check-expect (sum '()) 0)
(check-expect (sum (cons 3 (cons 4 (cons 2 '())))) 9)
(define (sum lon)
  (cond [(empty? lon) 0]
        [(cons? lon)
         (+ (first lon)         
            (sum (rest lon)))]))

;; prod : LoN -> Number
;; Compute the product of all given numbers.
(check-expect (prod '()) 1)
(check-expect (prod (cons 3 (cons 4 (cons 2 '())))) 24)
(define (prod lon)
  (cond [(empty? lon) 1]
        [(cons? lon)
         (* (first lon)
            (prod (rest lon)))]))

;; add1-all : LoN -> LoN
;; Compute list which is one more than every number in given list.
(check-expect (add1-all '()) '())
(check-expect (add1-all (cons 3 (cons 4 (cons 5 '()))))
              (cons 4 (cons 5 (cons 6 '()))))
(define (add1-all lon)
  (cond [(empty? lon) '()]
        [(cons? lon)
         (cons (add1 (first lon))
               (add1-all (rest lon)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;    +---------------------------------+
;;    |                                 |
;;    v                                 |
;; A LoS (list of strings) is one of:   |
;; - '()                                |
;; - (cons String LoS)                  |
;;                 |                    |
;;                 +--------------------+

;; contains-fred? : LoS -> Boolean
;; Are any of the strings "Fred"?
(define (contains-fred? los) #false)

;; count-length : LoS -> Natural
;; Compute the total length of all strings in the list.
(define (count-length los) 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;    +--------------------------------------------+
;;    |                                            |
;;    v                                            |
;; A LoLoN (list of list of numbers) is one of:    |
;; - '()                                           |
;; - (cons LoN LoLoN)                              |
;;          |    |                                 |
;;   +------+    +---------------------------------+
;;   |
;;   | +---------------------------------+
;;   | |                                 |
;;   v v                                 |
;; A LoN (list of numbers) is one of:    |
;; - '()                                 |
;; - (cons Number LoN)                   |
;;                 |                     |
;;                 +---------------------+

(define (lolon-template lln)
  (cond [(empty? lln) ...]
        [(cons? lln)
         (... (lon-template (first lln))
              ...
              (lolon-template (rest lln))
              ...)]))
  

;; sum-lists : LoLoN -> Number
;; Sum all of the numbers in all of the lists.
(define (sum-lists lln) 0)

;; sum-each-list : LoLoN -> LoN
;; Compute a list of sums for each list of numbers.
(define (sum-each-list lln) '())

