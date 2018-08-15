#lang class/0

;; A Student is a (new student% Name String)
(define-class student%
  (fields name uid))

;; A Name is a (new name% String String)
(define-class name%
  (fields first last))

(define stu
  (new student% (new name% "Stu" "Wert") "1234567"))

;; Write an expression that produces the first name of student stu.
