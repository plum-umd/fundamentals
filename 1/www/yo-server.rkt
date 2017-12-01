;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname yo-server) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)

(universe #false ; the server remembers nothing
          [on-new (λ (u iw) u)]                    
          [on-msg
           (λ (u iw msg)
             (make-bundle u
                          (list (make-mail iw (string-append "yo: " msg)))
                          '()))])
                    
                    