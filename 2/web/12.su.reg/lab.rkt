#lang racket
(provide exercise-counter
         lab:section)
(require scribble/base scribble/core scribble/html-properties)

(define (exercise-counter)
  (let [(i 0)]
    (λ xs
      (set! i (+ 1 i))
      (apply nested #:style "exercise"
                    (bold (format "Exercise ~s." i))
                    " "
                    xs))))

(define (lab:section . args)
  ; TODO Report bug: #:style has no effect with section
  #;(apply section #:style (style "labheading" (list (css-addition "2510H.css"))) args)
  (elem (first args) #:style "labheading"))
