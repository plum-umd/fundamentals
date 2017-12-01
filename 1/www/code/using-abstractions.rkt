;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname using-abstractions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; add-n-to-all : [Listof Number] Number -> [Listof Number]
;; Add n to every number in the list
(check-expect (add-n-to-all '() 5) '())
(check-expect (add-n-to-all (list 1 2 3) 5) (list 6 7 8))
(define (add-n-to-all lon n)
  (local [;; addn : Number -> Number
          ;; Add n to the number
          (define (addn x) (+ x n))]
    (map addn lon)))


;; negate : [X] (X -> Boolean) -> (X -> Boolean)
;; Produce a predicate that is the negation of the given predicate

(define (negate p)
  (local [;; X -> Boolean
          (define (not-p x)
            (not (p x)))]
    not-p))
(define not-even? (negate even?))
(check-expect (not-even? 3) #true)
(check-expect (not-even? 4) #false)


;; filter-not : [X] (X -> Boolean) [Listof X] -> [Listof X]
(define (filter-not p lox)
  (cond [(empty? lox) '()]
        [(cons? lox)
         (cond [(p (first lox)) (filter-not p (rest lox))]
               [else (cons (first lox)
                           (filter-not p (rest lox)))])]))

;; remove-all-ns : [Listof Number] Number -> [Listof Number]
;; Remove all occurrences of n in the list
(check-expect (remove-all-ns '() 5) '())
(check-expect (remove-all-ns (list 5 1 2 5 3 5) 5) (list 1 2 3))
(define (remove-all-ns lon n)
  (local [;; n? : Number -> Boolean
          ;; Is the number n?
          (define (n? x) (= n x))]
    (filter (negate n?) lon))

  #;
  (cond [(empty? lon) '()]
        [(cons? lon)
         (cond [(= (first lon) n) 
                (remove-all-ns (rest lon) n)]
               [else
                (cons (first lon)
                      (remove-all-ns (rest lon) n))])]))








