;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname drill-soln) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming with functions

;; even-pos? : Number -> Boolean
;; Is the number even and positive?
(check-expect (even-pos? 4) #true)
(check-expect (even-pos? 3) #false)
(check-expect (even-pos? -1) #false)
#;
(define (even-pos? n)
  (and (even? n) (positive? n)))

;; neg-odd? : Number -> Boolean
;; Is the number negative and odd?
(check-expect (neg-odd? 4) #false)
(check-expect (neg-odd? 3) #false)
(check-expect (neg-odd? -1) #true)
#;
(define (neg-odd? n)
  (and (negative? n) (odd? n)))

;; andf : [X] [X -> Boolean] [X -> Boolean] X -> Boolean
;; Produces true when both predicates are true
(define (andf p1 p2 x)
  (and (p1 x) (p2 x)))

(define (even-pos? n) (andf even? positive? n))
(define (neg-odd? n)  (andf negative? odd? n))

;; string-lower-alpha? : String -> Boolean
;; Is the string made up of lower case, alphabetic letters?
(check-expect (string-lower-alpha? "ABC") #false)
(check-expect (string-lower-alpha? "a2c") #false)
(check-expect (string-lower-alpha? "abc") #true)
(define (string-lower-alpha? s)
  (andf string-lower-case? string-alphabetic? s))

(λ (n) (andf even? (λ (n) (> n 5)) n))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signatures

;; lengths : [X] [Listof [Listof X]] -> [Listof Number]
;; total-length : [X] [Listof [Listof X]] -> Number
;; map-f-zero : [X] [Listof [Number -> X]] -> [Listof X]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using list abstractions

(check-expect (rev-append (list "there" "hi")) "hithere")
(define (rev-append los)
  (foldr (λ (s rs) (string-append rs s)) "" los))

(check-expect (posns-at-x (list 1 2 3) 7)
              (list (make-posn 7 1) (make-posn 7 2) (make-posn 7 3)))
(define (posns-at-x ys x)
  (map (λ (y) (make-posn x y)) ys))

(check-expect (dist (make-posn 0 0) (make-posn 3 4)) 5)
(define (dist p q)
  (sqrt (+ (sqr (- (posn-x p)
                   (posn-x q)))
           (sqr (- (posn-y p)
                   (posn-y q))))))
 
(check-expect (close-by (make-posn 0 0) (list (make-posn 3 4) (make-posn 8 8))
                        6)
              (list (make-posn 3 4)))
(define (close-by p lop d)
  (filter (λ (r) (<= (dist p r) d)) lop))

(check-expect (draw-on (list (make-posn 50 50)))
              (place-image (circle 10 "solid" "red")
                           50 50
                           (empty-scene 100 100)))
(define (draw-on lop)
  (foldr (λ (p img)
           (place-image (circle 10 "solid" "red") (posn-x p) (posn-y p) img))
         (empty-scene 100 100)
         lop))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Designing functions

;; dot-product : [Listof Number] [Listof Number] -> Number
;; Compute the dot product of the two lists
;; Assume: lists have same length
(check-expect (dot-product (list 1 2 3) (list 4 5 6))
              (+ (* 1 4) (* 2 5) (* 3 6)))
(define (dot-product l1 l2)
  (cond [(empty? l1) 0]
        [(cons? l2)
         (+ (* (first l1) (first l2))
            (dot-product (rest l1) (rest l2)))]))

;; outer-product : [Listof Number] [Listof Number] -> [Listof [Listof Number]]
;; Compute the outer product of the two lists
(check-expect (outer-product (list 1 2 3) '()) '())
(check-expect (outer-product '() (list 4 5 6 7)) '())
(check-expect (outer-product (list 1 2 3) (list 4 5 6 7))
              (list (list 4 5 6 7)
                    (list 8 10 12 14)
                    (list 12 15 18 21)))
#;
(define (outer-product l1 l2)
  (cond [(empty? l1) '()]
        [(empty? l2) '()]
        [(cons? l1)
         (cons (map (λ (n) (* (first l1) n)) l2)
               (outer-product (rest l1) l2))]))

;; outer-string-append : [Listof String] [Listof String]
;;                       -> [Listof [Listof String]]
;; Compute the outer string-append "product" of the two lists
(check-expect (outer-string-append (list "a" "b" "c") '()) '())
(check-expect (outer-string-append '() (list "1" "2" "3" "4")) '())
(check-expect (outer-string-append (list "a" "b" "c") (list "1" "2" "3" "4"))
              (list (list "a1" "a2" "a3" "a4")
                    (list "b1" "b2" "b3" "b4")
                    (list "c1" "c2" "c3" "c4")))
#;
(define (outer-string-append l1 l2)
  (cond [(empty? l1) '()]
        [(empty? l2) '()]
        [(cons? l1)
         (cons (map (λ (n) (string-append (first l1) n)) l2)
               (outer-string-append (rest l1) l2))]))


;; outer : [X Y Z] [X Y -> Z] [Listof X] [Listof Y] -> [Listof [Listof Z]]
;; Abstraction of outer "product" computations
(define (outer f l1 l2)
  ;; Simplified code use nested map:
  (cond [(empty? l2) '()]
        [else (map (λ (m) (map (λ (n) (f m n)) l2)) l1)])
  #;
  (cond [(empty? l1) '()]
        [(empty? l2) '()]
        [(cons? l1)
         (cons (map (λ (n) (f (first l1) n)) l2)
               (outer f (rest l1) l2))]))

(define (outer-product l1 l2) (outer * l1 l2))
(define (outer-string-append l1 l2) (outer string-append l1 l2))

;; append-to-each : String [Listof String] -> [Listof String]
;; Append string to each element of list
(check-expect (append-to-each "a" (list "a" "b" "c"))
              (list "aa" "ab" "ac"))
(define (append-to-each s los)
  (map (λ (t) (string-append s t)) los)
  #;
  (cond [(empty? los) '()]
        [(cons? los)
         (cons (string-append s (first los))
               (append-to-each s (rest los)))]))

;; append-all : [Listof String] [Listof String] -> [Listof String]
;; Append all of strings in los1 to all strings in los2
(check-expect (append-all (list "a" "b" "c") (list "1" "2"))
              (list "a1" "a2" "b1" "b2" "c1" "c2"))
(define (append-all los1 los2)
  (foldr (λ (s los) (append (append-to-each s los2) los)) '() los1)
  #;
  (cond [(empty? los1) '()]
        [(cons? los1)
         (append (append-to-each (first los1) los2)
                 (append-all (rest los1) los2))]))

;; brute-n : [Listof 1String] Natural -> [Listof String]
;; Compute all strings of given length composed out of given letters
(check-expect (brute-n (list "a" "b" "c") 2)
              (list "aa" "ab" "ac"
                    "ba" "bb" "bc"
                    "ca" "cb" "cc"))
(define (brute-n los n)
  (cond [(zero? n) (list "")]
        [else
         (append-all los (brute-n los (sub1 n)))]))

;; brute : [Listof 1String] Natural -> [Listof String]
;; Compute all strings of at most given length composed out of given letters
(check-expect (brute (list "a" "b" "c") 2)
              (list "" "a" "b" "c"
                    "aa" "ab" "ac"
                    "ba" "bb" "bc"
                    "ca" "cb" "cc"))
(define (brute los n)
  (cond [(= n 0) (list "")]
        [else
         (append (brute los (sub1 n))
                 (brute-n los n))]))
