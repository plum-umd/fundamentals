#lang racket

(define (object message)
  (cond [(string=? message "hello") "goodbye"]
        [else (error 'whoops)]))

(define (object%)
  (define (this other-this message)
    (cond [else (error 'whoops)]))
  this)

(define (shape%)
  (define super (object%))
  (define (methods this message)
    (cond
      [(string=? message "cost")
       (* 3 (this this "area"))]
      [else (super this message)]))
  methods)

;; new-square : Class -> Class
(define (make-square% super%)
  (define (square% size)
    (define length (* 2 size))
    (define super (super%))
    (define (methods this message)
      (cond [(string=? message "side") (λ () length)]
            [(string=? message "area") (λ () (* length length))]
            [(string=? message "cost")
             (λ (price) (* price (this this "area")))]
            [else (super this message)]))
    methods)
  square%)

(define square% (make-square% shape%))
(define square%2 (make-square% object%))

(square%2 17)

(define (new-empty-square size)
  (define super (square% size))
  (define (methods this message)
    (cond [(string=? message "area")
           (- (super this "area") 10)]
          [else (super this message)]))
  methods)

(object "hello")

(define square-10 (square% 5))
(square-10 square-10 "side")
(square-10 square-10 "area")
(square-10 square-10 "cost")

(define mt-10 (new-empty-square 5))
(mt-10 mt-10 "cost")

