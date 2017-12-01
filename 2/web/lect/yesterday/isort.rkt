;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname isort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A LoN is one of:
;; - empty
;; - (cons Number LoN)

;; Sort list of numbers in ascending order
;; LoN -> LoN

(define (gen-template in)
  (cond [(trivial1? in) solution1]
        [(trivial2? in) solution2]
        ...
        [(complex1? in)
         (put-together
          (gen-template (gen-subproblem1 in))
          (gen-template (gen-subproblem2 in))
          ...)]))
         

(define (qsort lon)
  (cond [(empty? lon) empty]
        [else
         (local [(define pivot (first lon))
                 (define biggers  (filter (λ (n) (> n pivot)) lon))
                 (define equals   (filter (λ (n) (= n pivot)) lon))
                 (define smallers (filter (λ (n) (< n pivot)) lon))]
           (append (qsort smallers)
                   equals
                   (qsort biggers)))]))
           

(check-expect (qsort (list 5)) (list 5))

(check-expect (qsort empty) empty)
(check-expect (qsort (list 3 2)) (list 2 3))


  