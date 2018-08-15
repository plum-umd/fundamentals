#lang class/0

;;   +- - - - - - - - - - - - - - +
;;   | +- - - - - - - - - - - - + |
;;   V V                        | |
;; A BT is one of:              | |
;; - (new leaf% Number)         | |
;; - (new node% Number BT BT)   | |
;;                     |  +- - -+ |
;;                     +- - - - --+


(define-class leaf%
  (fields number)

  ;; count : -> Number
  ;; Count the number of numbers in this leaf
  (check-expect (send (new leaf% 5) count) 1)
  (define (count)
    1))

(define-class node%
  (fields number left right)
  
  ;; count : -> Number
  ;; Count the number of numbers in this node
  (check-expect (send (new node% 7 (new leaf% 2) (new leaf% 1)) count) 3)
  (check-expect (send (new node% 8
                           (new node% 7 (new leaf% 2) (new leaf% 1))
                           (new leaf% 9))
                      count)
                5)
  (define (count)
    (+ 1
       (send (send this left) count)
       (send (send this right) count))))

(define A (new leaf% 4))

  