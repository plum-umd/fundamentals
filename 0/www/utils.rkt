#lang racket
(provide exercise float-right panopto-vid make-exercise make-exerciser strike)
(require scribble/base scribble/core scribble/html-properties)

(define exercise-body-style
  (make-style "ExerciseBody" null))

(define exercise-style
  (make-style "Exercise" null))

(define (make-exerciser prefix)
  (define *count* 0)
  (define (exercise title . t) 
    (set! *count* (add1 *count*))
    (nested #:style exercise-body-style 
            (para #:style exercise-style (format "~a ~a: " prefix *count*) title)
            t))
  exercise)

(define (make-exercise) (make-exerciser "Exercise"))

(define exercise (make-exercise))


(define float-right
  (style #f (list (attributes '((style . "float: right"))))))

(define (strike e)
  (elem #:style (style #f (list (attributes '((style . "text-decoration: line-through"))))) 
        e))


;; Embed a public panopto video into page
(define (panopto-vid src)
  (elem #:style 
        (style #f (list (alt-tag "iframe") 
                        (attributes 
                          `((src . ,src)
                            (width . "688")
                            (height . "387")
                            (gesture . "media")
                            (allowfullscreen . "")
                            (style . "padding: 0px; border: 1px solid #464646;")))))))

