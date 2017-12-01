;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bundle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; bundle : [Listof 1String] Natural -> [Listof String]
;; bundles chunks of s into strings of length n
(check-expect (bundle (list "a" "b" "c" "d") 1) (list "a" "b" "c" "d"))
(check-expect (bundle (list "b" "c" "d") 2)     (list "bc" "d"))
(check-expect (bundle (list "b" "c" "d") 1)     (list "b" "c" "d"))
(check-expect (bundle (list "a" "b" "c" "d") 2) (list "ab" "cd"))
(check-expect (bundle (list "c" "d") 2)         (list "cd"))
(check-expect (bundle (list "a" "b" "c" "d") 4) (list "abcd"))
(check-expect (bundle '() 2) '())
(define (bundle s n)
  (cond [(empty? s) '()]
        [else
         (cons (implode (take s n))
               (bundle (drop s n) n))]))

;; take : [X] [Listof X] Natural -> [Listof X]
;; Takes n elements of the list if available, or the whole thing
(define (take xs n)
  (cond [(empty? xs) '()]
        [(zero? n) '()]
        [(cons? xs)
         (cons (first xs)
               (take (rest xs) (sub1 n)))]))

;; drop : [X] [Listof X] Natural -> [Listof X]
;; Drops n elements of the list if available, or the whole thing
(define (drop xs n)
  (cond [(empty? xs) '()]
        [(zero? n) xs]
        [(cons? xs)
         (drop (rest xs) (sub1 n))]))
         