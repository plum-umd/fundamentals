#lang class/1

;; An Elem is one of
;; - (new file% String Number)
;; - (new dir% String LoElem)
;; implements
;; - list-files : -> LoElem

;; A LoElem is one of
;; - (new mt%)
;; - (new cons% Elem LoElem)
;; implements
;; - list-files : -> LoElem
;; - append : LoElem -> LoElem

(define-class dir%
  (fields name elems)
  (define (list-files)
    (this . elems . list-files)))

(define-class file%
  (fields name size)  
  (define (list-files)
    (new cons% this (new mt%))))

(define-class mt%
  (define (append loe) loe)
  (define (list-files) this))

(define-class cons%
  (fields first rest)
  (define (append loe)
    (new cons% (this . first) (this . rest . append loe)))
  (define (list-files)
    (this . first . list-files . append (this . rest . list-files))))
    
(define mt (new mt%))
(define a (new file% "a.txt" 5))
(define b (new file% "b.rkt" 10))
(define c (new file% "c.c" 1000))

(check-expect (mt . append mt) mt)
(check-expect (mt . append (new cons% a mt)) (new cons% a mt))
(check-expect ((new cons% a mt) . append (new cons% b mt))
              (new cons% a (new cons% b mt)))

(check-expect (mt . list-files) mt)
(check-expect ((new cons% a (new cons% b mt)) . list-files)
              (new cons% a (new cons% b mt)))

(check-expect (a . list-files) (new cons% a mt))

(define C/ (new dir% "C" (new cons% c mt)))
(define B/ (new dir% "B" (new cons% b mt)))
(define A/ (new dir% "A" (new cons% a (new cons% B/ mt))))

#|
├── A
│   ├── a.txt
│   └── B
│       └── b.rkt
└── C
   └── c.c
|#
(check-expect 
 ((new dir% "/" (new cons% A/ (new cons% C/ mt))) . list-files)
 (new cons% a (new cons% b (new cons% c mt))))



