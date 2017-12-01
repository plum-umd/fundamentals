;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname traffic) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Design a program that simulates the operation of a
;; traffic light.
;; Assume light changes every 3 seconds.

;; A Light is one of:
;; - "red"
;; - "green" 
;; - "yellow"
;; Interp: color of the light

;; Simulate a traffic light starting from given state.
;; Light -> Light
(define (main light)
  (big-bang light
            [on-tick next]
            [to-draw draw]))

;; next : Light -> Light
;; computes the next light in the sequence
(check-expect (next "red") "green")
(check-expect (next "green") "yellow")
(check-expect (next "yellow") "red")
#;(define (next light) "red")
(define (next light)
  (cond [(string=? "red" light) "green"]
        [(string=? "yellow" light) "red"]
        [(string=? "green" light) "yellow"]))


;; draw : Light -> Image
;; Draw a light
(check-expect (draw "red") (overlay (circle 50 "solid" "red")
                                    (empty-scene 100 100)))
(define (draw light)
  (overlay (circle 50 "solid" "red")
           (empty-scene 100 100)))





  




