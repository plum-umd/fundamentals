;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rel-abs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; rel->abs
;; LoN -> LoN
#;
(define (rel->abs ls)
  (cond [(empty? ls) empty]
        [(cons? ls)
         (local [(define (add-first n)
                   (+ (first ls) n))]
           (cons (first ls)
                 (map add-first
                      (rel->abs (rest ls)))))]))

(define (rel->abs ls) (rel->abs/a ls 0))

;; LoN Number -> LoN
;; Accum: a represents the absolute distance traveled so far.
(define (rel->abs/a ls a)
  (cond [(empty? ls) empty]
        [(cons? ls)
         (cons (+ (first ls) a)
               (rel->abs/a (rest ls) (+ (first ls) a)))]))
         
(check-expect (rel->abs (list 2 3))
              (list 2 5))

(check-expect (rel->abs (list 1 2 3))
              (list 1 3 6))









