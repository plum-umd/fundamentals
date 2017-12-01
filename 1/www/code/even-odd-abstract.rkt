;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname eo-stub) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; evens : LoN -> LoN
;; Keep only the even? elements of the list
(check-expect (evens '()) '())
(check-expect (evens (list 1 2 3 4)) (list 2 4))
(define (evens l)
  (eos even? l))

;; odds : LoN -> LoN
;; Keep only the odd? elements of the list
(check-expect (odds '()) '())
(check-expect (odds (list 1 2 3 4)) (list 1 3))
(define (odds l)
  (eos odd? l))

;; eos : (Number -> Boolean) LoN -> LoN
(define (eos huh? l)
  (cond [(empty? l) '()]
        [(cons? l)
         (cond [(huh? (first l))
                (cons (first l)
                      (eos huh? (rest l)))]
               [else
                (eos huh? (rest l))])]))

(define (bigger-than-one? n) (> n 1))