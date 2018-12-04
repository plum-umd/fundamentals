#lang racket
(provide exercise)
(require scribble/base scribble/core scribble/html-properties)

(define exercise-body-style
  (make-style "ExerciseBody" null))

(define exercise-style
  (make-style "Exercise" null))

(define *count* 0)

(define (exercise title . t) 
  (set! *count* (add1 *count*))
  (nested #:style exercise-body-style 
          (para #:style exercise-style (format "Exercise ~a:" *count*))
          t))

