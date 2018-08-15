;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bt-fun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;   +- - - - - - - - - - - - - - +
;;   | +- - - - - - - - - - - - + |
;;   V V                        | |
;; A BT is one of:              | |
;; - (make-leaf Number)         | |
;; - (make-node Number BT BT)   | |
;;                     |  +- - -+ |
;;                     +- - - - --+
(define-struct leaf (number))
(define-struct node (number left right))

;; count : BT -> Natural
;; Count the number of numbers in given tree
(define (count bt)
  ...)

;; double : BT Number -> BT
;; Make new tree with given number and duplicate given BT as subtrees
(define (double bt n)
  ...)
