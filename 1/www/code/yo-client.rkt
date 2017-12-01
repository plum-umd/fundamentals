;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname yo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define (yo _)
  (big-bang "yo!"
            [register LOCALHOST]
            [to-draw (λ (w)
                       (overlay (text w 40 "black")
                                (empty-scene 600 200)))]
            [on-key (λ (w ke)
                      (make-package w w))]
            [on-receive (λ (w msg) msg)]))
                         

(define (run _)
   (launch-many-worlds (yo 5)
                       (yo 3)
                       (yo 1)))