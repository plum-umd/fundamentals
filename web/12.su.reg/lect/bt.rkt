;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A BT is one of:
;; - (make-leaf Number)
;; - (make-node BT BT)

(define-struct leaf (data))
(define-struct node (left right))

;; bt-template : BT -> ?
#;
(define (bt-template bt)
  (cond [(leaf? bt) ... (leaf-data bt) ...]
        [(node? bt) ... 
         (bt-template (node-left bt)) ...
         (bt-template (node-right bt))]))

;; count-leaves : BT -> Number
;; Count the number of leaves in the given tree.
(check-expect (count-leaves (make-leaf 4)) 1)
(check-expect (count-leaves (make-node (make-leaf 5) (make-leaf 6))) 2)
(define (count-leaves bt)
  (cond [(leaf? bt) 1]
        [(node? bt) 
         (+ (count-leaves (node-left bt)) 
            (count-leaves (node-right bt)))]))

;; maximum : BT -> Number
;; Produce the maximal element of a given tree.
(check-expect (maximum (make-leaf 4)) 4)
(check-expect (maximum (make-node (make-leaf 5) (make-leaf 6))) 6)
(define (maximum bt)
  (cond [(leaf? bt) (leaf-data bt)]
        [(node? bt) 
         (max (maximum (node-left bt))
              (maximum (node-right bt)))]))

;; ordered? : BT -> Boolean
;; Are all the elements of the given tree in non-decreasing order from left to right.
(check-expect (ordered? (make-leaf 4)) true)
(check-expect (ordered? (make-node (make-leaf 5) (make-leaf 6))) true)
(check-expect (ordered? (make-node (make-leaf 5) (make-leaf 5))) true)
(check-expect (ordered? (make-node (make-leaf 6) (make-leaf 5))) false)
(check-expect (ordered? (make-node (make-node (make-leaf 5) (make-leaf 6))
                                   (make-node (make-leaf 5) (make-leaf 6))))
              false)

(define (ordered? bt)
  (cond [(leaf? bt) true]
        [(node? bt) 
         (and (ordered? (node-left bt))
              (ordered? (node-right bt))
              (<= (rightmost (node-left bt))
                  (leftmost (node-right bt))))]))

;; rightmost : BT -> Number
;; Get the right-most element of the given tree.
(define (rightmost bt)
  (cond [(leaf? bt) (leaf-data bt)]
        [(node? bt)
         (rightmost (node-right bt))]))

;; leftmost : BT -> Number
;; Get the left-most element of the given tree.
(define (leftmost bt)
  (cond [(leaf? bt) (leaf-data bt)]
        [(node? bt)
         (leftmost (node-left bt))]))

