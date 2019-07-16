;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname invader) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main

;; main : Number -> Game
;; Launches a game of Space Invaders (simplified)
;; Example: (main 0)
(define (main n)
  (big-bang (make-si (+ n (* 1/2 WIDTH)) #false)
    [to-draw si-draw]
    [on-tick si-advance]
    [on-key si-handle-key]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A SI (Space Invader game) is a (make-si Base MaybeShot)
;; Interpretation: x is the x-coordinate of the base, s is the current shot 
(define-struct si (x s))

;; A Base is an Integer

;; A MaybeShot is one of:
;; - #false
;; - Shot
;; Interpretation: shot or #false to indicate no shot

;; A Shot is a (make-posn Integer Integer)
;; Intepretation: coordinates of a shot position

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defined Constants

(define WIDTH 500)  ; pixels
(define HEIGHT 500) ; pixels
(define SHOT-SIZE 10) ; pixels
(define SHOT-SPEED 5) ; pixels/tick
(define SHOT-IMG (triangle SHOT-SIZE "solid" "orange"))
(define SHOT-INIT-Y (- HEIGHT 10))
(define BASE-SPEED 8)
(define BASE-HEIGHT 20)
(define BASE-IMAGE (rectangle 70 BASE-HEIGHT "solid" "green"))
(define BASE-Y (- HEIGHT (* 1/2 BASE-HEIGHT)))
(define SCN (empty-scene WIDTH HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SI functions

;; SI -> Image
;; Draw the space invader game
(check-expect (si-draw (make-si 8 (make-posn 12 10)))
              (base-draw-on 8 (shot-draw-on (make-posn 12 10) SCN)))
(define (si-draw si)
  (base-draw-on (si-x si)
                (maybe-shot-draw-on (si-s si) SCN)))

;; SI -> SI
;; Advance the shots upward
(check-expect (si-advance (make-si 8 #false)) (make-si 8 #false))
(check-expect (si-advance (make-si 8 (make-posn 5 5)))
              (make-si 8 (make-posn 5 (- 5 SHOT-SPEED))))
(check-expect (si-advance (make-si 8 (make-posn 5 -10)))
              (make-si 8 #false))
(define (si-advance si)
  (make-si (si-x si) (maybe-shot-advance/remove (si-s si))))

;; SI KeyEvent -> SI
;; Fire a shot (if possible) on any key event
(check-expect (si-handle-key (make-si 8 #false) " ")
              (make-si 8 (make-posn 8 SHOT-INIT-Y)))
(check-expect (si-handle-key (make-si 8 (make-posn 10 12)) " ")
              (make-si 8 (make-posn 10 12)))
(check-expect (si-handle-key (make-si 8 #false) "left")
              (make-si (- 8 BASE-SPEED) #false))
(check-expect (si-handle-key (make-si 8 #false) "right")
              (make-si (+ 8 BASE-SPEED) #false))
(check-expect (si-handle-key (make-si 8 #false) "a")
              (make-si 8 #false))
(define (si-handle-key si ke)
  (cond [(key=? ke " ")
         (make-si (si-x si) (maybe-shot-new (si-s si) (si-x si)))]
        [(key=? ke "left")
         (make-si (base-move-left (si-x si)) (si-s si))]
        [(key=? ke "right")
         (make-si (base-move-right (si-x si)) (si-s si))]
        [else si]))       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MaybeShot functions

;; MaybeShot Image -> Image
;; Draw possible shot on image
(check-expect (maybe-shot-draw-on #false (empty-scene 50 50)) (empty-scene 50 50))
(check-expect (maybe-shot-draw-on (make-posn 8 10) (empty-scene 50 50))
              (shot-draw-on (make-posn 8 10) (empty-scene 50 50)))
(define (maybe-shot-draw-on ms scn)
  (cond [(false? ms) scn]
        [else (shot-draw-on ms scn)]))

;; MaybeShot -> MaybeShot
;; Fire a shot from x if possible
(check-expect (maybe-shot-new #false 8) (shot-new 8))
(check-expect (maybe-shot-new (make-posn 10 12) 8) (make-posn 10 12))
(define (maybe-shot-new ms x)
  (cond [(false? ms) (shot-new x)]
        [else ms]))

;; MaybeShot -> MaybeShot
;; Advance a possible shot upward, remove if off screen
(check-expect (maybe-shot-advance/remove #false) #false)
(check-expect (maybe-shot-advance/remove (make-posn 8 -100)) #false)
(check-expect (maybe-shot-advance/remove (make-posn 8 10))
              (shot-advance (make-posn 8 10)))
(define (maybe-shot-advance/remove ms)
  (cond [(false? ms) #false]
        [(shot-offscreen? ms) #false]
        [else (shot-advance ms)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shot functions

;; Shot Image -> Image
;; Draw shot on given image
(check-expect (shot-draw-on (make-posn 8 10) (empty-scene 50 50))
              (place-image SHOT-IMG 8 10 (empty-scene 50 50)))
(define (shot-draw-on s scn)
  (place-image SHOT-IMG (posn-x s) (posn-y s) scn))

;; Integer -> Shot
;; Create a new shot at given x coordinate
(check-expect (shot-new 8) (make-posn 8 SHOT-INIT-Y))
(define (shot-new x)
  (make-posn x SHOT-INIT-Y))

;; Shot -> Shot
;; Advance the shot upward
(check-expect (shot-advance (make-posn 8 HEIGHT))
              (make-posn 8 (- HEIGHT SHOT-SPEED)))
(define (shot-advance s)
  (make-posn (posn-x s) (- (posn-y s) SHOT-SPEED)))

;; Shot -> Boolean
;; Is the shot off-screen?
(check-expect (shot-offscreen? (make-posn 8 10)) #false)
(check-expect (shot-offscreen? (make-posn 8 -100)) #true)
(define (shot-offscreen? s)
  (< (posn-y s) (- (image-height SHOT-IMG))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base functions

;; Base Image -> Image
;; Draw base on given image
(check-expect (base-draw-on 10 (empty-scene 50 50))
              (place-image BASE-IMAGE 10 BASE-Y (empty-scene 50 50)))
(define (base-draw-on b scn)
  (place-image BASE-IMAGE b BASE-Y scn))

;; Base -> Base
;; Move the base left
(check-expect (base-move-left 10) (- 10 BASE-SPEED))
(define (base-move-left b)
  (- b BASE-SPEED))

;; Base -> Base
;; Move the base right
(check-expect (base-move-right 10) (+ 10 BASE-SPEED))
(define (base-move-right b)
  (+ b BASE-SPEED))




