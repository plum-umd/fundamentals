#lang class/2

(define-class q%
  (fields front back)
  (define (enq e)
    (cond [(this . emp?)
           (new q% (list e) empty)]
          [else
           (new q% (this . front) (cons e (this . back)))]))
  (define (deq)
    (cond [(empty? (rest (this . front)))
           (new q% (reverse (this . back)) empty)]
          [else (new q% (rest (this . front)) (this . back))]))
  (define (head)
    (first (this . front)))
  (define (emp?) (empty? (this . front))))

(define-class broq%
  (fields front back)
  (define (enq e)
    (cond [(this . emp?)
           (new broq% (list e) empty)]
          [else
           (new broq% (this . front) (cons e (this . back)))]))
  (define (deq)
    (cond [(empty? (rest (this . front)))
           (new broq% (reverse (this . back)) (this . back))]
          [else (new broq% (rest (this . front)) (this . back))]))
  (define (head)
    (first (this . front)))
  (define (emp?) (empty? (this . front))))

((new q% empty empty) . emp?)

(define q (new q% '(1 2 3) empty))
(define eq (new q% empty empty))

(check-expect(q . emp?) false)
(check-expect(q . head) 1)
(check-expect(eq . emp?) true)
(check-expect(eq . enq 1) (new q% '(1) '()))
(check-expect(eq . enq 1 . deq) eq)
(check-expect(q . enq 5) (new q% '(1 2 3) '(5)))
(check-expect (q . enq 5 . deq) (new q% '(2 3) '(5)))


(define q2 (new broq% '(1 2 3) empty))
(define eq2 (new broq% empty empty))
(define bq eq2)

(check-expect(q2 . emp?) false)
(check-expect(q2 . head) 1)
(check-expect(eq2 . emp?) true)
(check-expect(eq2 . enq 1) (new broq% '(1) '()))
(check-expect(eq2 . enq 1 . deq) eq2)
(check-expect(q2 . enq 5) (new broq% '(1 2 3) '(5)))
(check-expect (q2 . enq 5 . deq) (new broq% '(2 3) '(5)))

(define (do f n)
  (cond [(zero? n) true]
        [else (local [(define r (f n))]
                (do f (sub1 n)))]))

(define (test-q _)
  (local [(define n (random 10))
          (define n2 (random 10))]
    (check-expect (bq . enq n . deq . enq n2 . head) n2)))

(do test-q 10)


