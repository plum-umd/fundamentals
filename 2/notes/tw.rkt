#lang class/2

(require class/universe 2htdp/image)

(require (only-in racket format))

(provide (all-defined-out))

(define-class w%
  (fields nn)
  (define (tick-rate) 1/5)(define (tickr) (this . tick-rate))
  
  (define (on-tick) (new w% (add1 (this . nn))))
  (define (to-draw) (overlay (text (number->string (this . nn)) 20 "red") (empty-scene 200 200)))
  (define (stop-when) false)
  (define (on-mouse e x y) this)
  (define (on-key k) (new w% 0)))

(define-class w*%
  (fields nn)
  (define (stop-when) false)(define (tickr) (this . tick-rate))
  (define (tick-rate) 1/10)
  (define (on-tick) (new w*% (add1 (this . nn))))
  (define (to-draw) (overlay (text (number->string (this . nn)) 20 "blue") (empty-scene 200 200)))
  (define (on-mouse e x y) this)
  (define (on-key k) (new w*% 0)))

(define-class w2%
  (fields tt)
  (define (tick-rate) 1)
  (define (stop-when) false)
  (define (on-tick) this)(define (tickr) (this . tick-rate))
  (define (to-draw) (overlay (text (this . tt) 20 "blue")
                             (empty-scene 200 200)))
  (define (on-mouse x y e) (new w2% (format "x:~a y:~a e:~a" x y e)))
  (define (on-key k) (new w2% k)))


