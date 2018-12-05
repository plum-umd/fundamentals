#lang racket
(provide exercise float-right)
(require scribble/base scribble/core scribble/html-properties)

(define exercise-body-style
  (make-style "ExerciseBody" null))

(define exercise-style
  (make-style "Exercise" null))

(define *count* 0)

(define (exercise title . t) 
  (set! *count* (add1 *count*))
  (nested #:style exercise-body-style 
          (para #:style exercise-style (format "Exercise ~a: " *count*) title)
          t))


(define float-right
  (style #f (list (attributes '((style . "float: right"))))))
