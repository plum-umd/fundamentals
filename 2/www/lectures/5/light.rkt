#lang class/0
(provide (all-defined-out))
(require 2htdp/image)

(define LIGHT-RADIUS 20)

(define-class red%
  ;; on-tick : -> Light
  ;; Next light after red
  #;(check-expect (send (new red%) on-tick) (new green%))
  (define (on-tick)
    (new green%))

  ;; to-draw : -> Image
  ;; Draw this red light
  #;(check-expect (send (new red%) to-draw)
		(circle LIGHT-RADIUS "solid" "red"))
  (define (to-draw)
    (circle LIGHT-RADIUS "solid" "red")))

(define-class green%
  ;; on-tick : -> Light
  ;; Next light after green
  #;(check-expect (send (new green%) on-tick) (new yellow%))
  (define (on-tick)
    (new yellow%))

  ;; to-draw : -> Image
  ;; Draw this green light
  #;(check-expect (send (new green%) to-draw)
		(circle LIGHT-RADIUS "solid" "green"))
  (define (to-draw)
    (circle LIGHT-RADIUS "solid" "green")))

(define-class yellow%
  ;; on-tick : -> Light
  ;; Next light after yellow
  #;(check-expect (send (new yellow%) on-tick) (new red%))
  (define (on-tick)
    (new red%))

  ;; to-draw : -> Image
  ;; Draw this yellow light
  #;(check-expect (send (new yellow%) to-draw)
		(circle LIGHT-RADIUS "solid" "yellow"))
  (define (to-draw)
    (circle LIGHT-RADIUS "solid" "yellow")))
