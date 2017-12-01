;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname shots) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Authors: dvanhorn
; Purpose: Simplified version of Space Invaders

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Shot is (make-posn Integer Integer)
;; Interp: the px coordinate of the shot

;; A Shots is one of:
;; - (make-no-shots)
;; - (make-some-shots Shot Shots)
(define-struct no-shots ())
(define-struct some-shots (first rest))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main

(define (shot-main i)
  (big-bang (make-posn i SCENE-HEIGHT)
            [to-draw shot-draw]
            [on-tick shot-tock]
            [stop-when shot-over-top?]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

(define SCENE-HEIGHT 200)
(define SCENE-WIDTH 500)
(define MT-SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))

(define SHOT-SIZE 15)
(define SHOT-IMAGE (triangle SHOT-SIZE "solid" "blue"))
(define SHOT-SPEED 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shots functions

(define (shots-template shots)
  (cond [(no-shots? shots) ...]
        [(some-shots? shots)
         (... (shot-template (shots-some-first shots))
              (shots-template (shots-some-rest shots))
              ...)]))

(define S0 (make-no-shots))
(define S1
  (make-some-shots (make-posn 30 40) (make-no-shots)))
(define S2
  (make-some-shots (make-posn 30 40)
                   (make-some-shots (make-posn 50 50)
                                    (make-some-shots (make-posn 80 70)
                                                     (make-no-shots)))))

;; shots-draw-on : Shots Image -> Image
;; Draw all of the shots on the given image
(check-expect (shots-draw-on S0 MT-SCENE) MT-SCENE)
(check-expect (shots-draw-on S0 (shot-draw-on (make-posn 90 10) MT-SCENE))
              (shot-draw-on (make-posn 90 10) MT-SCENE))              
(check-expect (shots-draw-on S1 MT-SCENE)
              (shot-draw-on (make-posn 30 40) MT-SCENE))
(check-expect (shots-draw-on S2 MT-SCENE)
              (shot-draw-on (make-posn 30 40)
                            (shot-draw-on (make-posn 50 50)
                                          (shot-draw-on (make-posn 80 70)
                                                        MT-SCENE))))
(define (shots-draw-on shots img)
  (cond [(no-shots? shots) img]
        [(some-shots? shots)
         (shot-draw-on (some-shots-first shots)
                       (shots-draw-on (some-shots-rest shots) img))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shot functions

(define (shot-template s)
  (... (posn-x s)
       (posn-y s) ...))

;; shot-draw : Shot -> Image
;; Draw shot on an empty scene
(check-expect (shot-draw (make-posn 100 200))
              (place-image SHOT-IMAGE 100 200 MT-SCENE))                        
(define (shot-draw s)
  (shot-draw-on s MT-SCENE))

;; shot-draw-on : Shot Image -> Image
;; Draw shot on given image
(check-expect (shot-draw-on (make-posn 100 200) MT-SCENE)
              (place-image SHOT-IMAGE 100 200 MT-SCENE))
(define (shot-draw-on s img)
  (place-image SHOT-IMAGE (posn-x s) (posn-y s) img))


;; shot-tock : Shot -> Shot
;; Advance the shot upward one tick of time
(check-expect (shot-tock (make-posn 100 200))
              (make-posn 100 (- 200 SHOT-SPEED)))
(define (shot-tock s)
  (make-posn (posn-x s)
             (- (posn-y s) SHOT-SPEED)))

;; shot-over-top? : Shot -> Boolean
;; Has the shot gone over the top of the scene
(check-expect (shot-over-top? (make-posn 100 200)) #false)
(check-expect (shot-over-top? (make-posn 100 0)) #false)
(check-expect (shot-over-top? (make-posn 100 -1)) #true)
(define (shot-over-top? s)
  (< (posn-y s) 0))

;; shot-in-bb? : Shot Integer Integer Integer Integer -> Boolean
;; Is the shot in the bounding box given by ul-x, ul-y, lr-x, lr-y?
(check-expect (shot-in-bb? (make-posn 0 0) 0 0 10 10) #true)
(check-expect (shot-in-bb? (make-posn 10 10) 0 0 10 10) #true)
(check-expect (shot-in-bb? (make-posn 5 5) 0 0 10 10) #true)
(check-expect (shot-in-bb? (make-posn 20 0) 0 0 10 10) #false)
(check-expect (shot-in-bb? (make-posn 0 20) 0 0 10 10) #false)
(define (shot-in-bb? s ul-x ul-y lr-x lr-y)
  (and (<= ul-x (posn-x s) lr-x)
       (<= ul-y (posn-y s) lr-y)))

