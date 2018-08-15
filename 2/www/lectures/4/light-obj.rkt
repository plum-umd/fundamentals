#lang class/0

(require class/universe)
(require 2htdp/image)

;; A Light implements:
;; - to-draw : -> Image
;; - on-tick : -> Light

(define SIZE 40)

(define-class light%
  (fields n) ;; 0 = yellow, 1 = red, 2 = green

  (define (tick-rate) 1)
  
  ;; to-draw : -> Image
  (define (to-draw)
    (circle SIZE "solid"
            (cond [(= (send this n) 0) "yellow"]
                  [(= (send this n) 1) "red"]
                  [(= (send this n) 2) "green"])))

  ;; on-tick : -> Light
  (check-expect (send (new light% 2) on-tick) (new light% 0))
  (define (on-tick)
    (new light% (modulo (add1 (send this n)) 3))))

(define-class red%
  ;; next : -> Light
  ;; Next light after this red light
  (check-expect (send (new red%) next) (new green%))
  (define (next)
    (new green%))

  (define (tick-rate) 1)

  ;; to-draw : -> Image
  ;; Render this red light
  (define (to-draw)
    (circle SIZE "solid" "red"))

  ;; on-tick : -> Light
  ;; Advance after clock tick
  (define (on-tick)
    (send this next)))


(define-class green%
  ;; next : -> Light
  ;; next light after this green light
  (check-expect (send (new green%) next) (new yellow%))
  (define (next)
    (new light% 0))

  (define (on-tick) (send this next))
  (define (to-draw) (circle SIZE "solid" "green")))

(define-class yellow%
  ;; next : -> Light
  ;; Next light after this yellow light
  (check-expect (send (new yellow%) next) (new red%))
  (define (next)
    (new red%))

  (define (on-tick) (send this next))
  (define (to-draw) (circle SIZE "solid" "yellow")))


(define-class blinky-yellow%
  ;; to-draw : -> Image
  (define (to-draw)
    (circle SIZE "solid" "yellow"))
  
  ;; on-tick : -> Light
  (define (on-tick)
    this))
  


(define (main)
  (big-bang (new red%)))

(define (main-yellow)
  (big-bang (new blinky-yellow%)))







