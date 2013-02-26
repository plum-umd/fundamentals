;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname foldl) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; sum : LoN -> Number
;; Sum the list of numbers
(define (sum-struct lon)
  (cond [(empty? lon) 0]
        [(cons? lon)
         (+ (first lon)
            (sum-struct (rest lon)))]))

(check-expect (sum-struct empty) 0)
(check-expect (sum-struct (list 3 4 5)) 12)

;; LoN -> Number
#;
(define (sum lon)
  ;; LoN Number -> Number
  ;; Sums lon and adds a.
  ;; ACCUM: sum of the numbers seen so far.
  (local [(define (sum-acc lon a)
            (cond [(empty? lon) a]
                  [(cons? lon)
                   (sum-acc (rest lon) (+ (first lon) a))]))]
    (sum-acc lon 0)))

(check-expect (sum empty) 0)
(check-expect (sum (list 3 4 5)) 12)

;; prod : LoN -> Number
;; Compute product of list
#;
(define (prod lon)
  ;; ACCUM: product of the numbers seen so far.
  (local [(define (prod-acc lon a)
            (cond [(empty? lon) a]
                  [(cons? lon)
                   (prod-acc (rest lon) (* (first lon) a))]))]
    (prod-acc lon 1)))

(check-expect (prod empty) 1)
(check-expect (prod (list 1 2 3)) 6)

;; [X Y -> Y] Y [List X] -> Y
(define (thing op init lon)
  (local [(define (thing-acc lon a)
            (cond [(empty? lon) a]
                  [(cons? lon)
                   (thing-acc (rest lon) (op (first lon) a))]))]
    (thing-acc lon init)))

(define (prod lon) (foldr * 1 lon))
(define (sum lon) (foldr + 0 lon))
(define (len los) (foldr (λ (s n) (add1 n)) 0 los))

(check-expect (len empty) 0)
(check-expect (len (list "fred" "wilma")) 2)





































  
  

  
  
  
  
  
  
  
  
  