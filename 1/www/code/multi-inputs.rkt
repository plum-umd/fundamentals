;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname today) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; tack : [Listof X] [Listof X] -> [Listof X]
;; Append the two given lists together
(check-expect (tack '() '()) '())
(check-expect (tack '() (list 1 2 3)) (list 1 2 3))
(check-expect (tack (list 1 2) (list 3 4 5))
              (list 1 2 3 4 5))
(define (tack l1 l2)
  (cond [(empty? l1) l2]
        [(cons? l1)
         (cons (first l1) 
               (tack (rest l1) l2))]))

;; zip : [X Y] [Listof X] [Listof Y] -> [Listof (make-posn X Y)]
;; Pair each element of the lists together
;; Assume: each list has the same length
(check-expect (zip '() '()) '())
(check-expect (zip (list 1 2) (list "a" "b"))
              (list (make-posn 1 "a") (make-posn 2 "b")))
(define (zip l1 l2)
  (cond [(empty? l1) '()]
        [(and (cons? l1) (cons? l2))
         (cons (make-posn (first l1)
                          (first l2))
               (zip (rest l1) (rest l2)))]))

;; A Natural is one of:
;; - 0
;; - (add1 Natural)

;; list-get : [X] [Listof X] Natural -> X
;; Get the ith element of the list, error if i is invalid
(check-error (list-get '() 0) "out of bounds")
(check-error (list-get '() 415) "out of bounds")
(check-expect (list-get (list "a" "b" "c") 0) "a")
(check-expect (list-get (list "a" "b" "c") 1) "b")
(check-expect (list-get (list "a" "b" "c") 2) "c")
(check-error (list-get (list "a" "b" "c") 3) "out of bounds")
(check-error (list-get (list "a" "b" "c") 317) "out of bounds")

(define (list-get xs i)
  (cond [(and (empty? xs) (zero? i)) (error "out of bounds")]
        [(empty? xs) (error "out of bounds")]
        [(zero? i)
         (first xs)]         
        [else
         (list-get (rest xs) (sub1 i))]))

