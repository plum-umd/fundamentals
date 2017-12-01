;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname eclipse) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define sun  (circle 100 "solid" "yellow"))
(define moon (circle 95 "solid" "black"))

(define (eclipse time)
  (place-image
   moon
   (- 500 time)  250
   (overlay sun (empty-scene 500 500))))

(define (main _)
  (animate eclipse))

