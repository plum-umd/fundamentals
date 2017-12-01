;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname sort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A LoN is one of:
;; - '()
;; - (cons Number LoN)

(define (lon-template lon)
  (cond [(empty? lon) ...]
        [(cons? lon)
         (... (first lon)
              (lon-template (rest lon))
              ...)]))

;; sort> : LoN -> LoN
;; Sort the given list in descending order
(check-expect (sort> '()) '())
(check-expect (sort> (list 4)) (list 4))
(check-expect (sort> (list 3 8 1 2)) (list 8 3 2 1))
(check-expect (sort> (cons 3 (cons 8 (cons 1 (cons 2 '())))))
              (cons 8 (cons 3 (cons 2 (cons 1 '())))))

(define (sort> lon)
  (cond [(empty? lon) '()]
        [(cons? lon)
         (insert (first lon)
                 (sort> (rest lon)))]))

;; insert : Number LoN -> LoN
;; Insert the given number into the appropriate place in a sorted list of #s.
(check-expect (insert 4 '()) (cons 4 '()))
(check-expect (insert 3 (cons 8 (cons 2 (cons 1 '()))))
              (cons 8 (cons 3 (cons 2 (cons 1 '())))))
(check-expect (insert 3 (cons 2 (cons 1 '())))
              (cons 3 (cons 2 (cons 1 '()))))
              
(define (insert n lon)
  (cond [(empty? lon) (cons n '())]
        [(cons? lon)
         (cond [(> n (first lon)) (cons n lon)]
               [else
                (cons (first lon)
                      (insert n (rest lon)))])]))
              










