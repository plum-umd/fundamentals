;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname assign1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Assignment 1
;; Name: ...your name here...

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main

;; main : Number -> Game
;; Launches a game of "Bug" played at given tick rate
;; Example: (main 1/10)
(define (main r)
  (big-bang G0
    [to-draw game-draw]
    [on-tick game-advance r]
    [on-key game-handle-key]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A Game is a (make-game Bug Food)
(define-struct game (bug food))

;; A Bug is a (make-bug Dir Posn)
(define-struct bug (dir posn))

;; A Food is a Posn

;; A Posn is a (make-posn Integer Integer)

;; A Dir is one of:
;; - "left"
;; - "right"
;; - "up"
;; - "down"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defined Constants

(define PX/U 20) ; pixels per unit
(define WIDTH 20)  ; units
(define HEIGHT 20) ; units

(define G0 ; an initial game state
  (make-game (make-bug "up"
                       (make-posn (quotient WIDTH 2)
                                  (quotient HEIGHT 2)))
             (make-posn 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game functions

;; game-handle-key : Game KeyEvent -> Game
;; Handle a key event in this game
(define (game-handle-key g ke)
  ; stub
  g)

;; game-advance : Game -> Game
;; Advance the bug, maybe eating the food
(define (game-advance g)
  ; stub
  g)

;; game-draw : Game -> Scene
;; Render the game as a scene
(define (game-draw g)
  ; stub
  (empty-scene (* WIDTH PX/U)
               (* HEIGHT PX/U)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bug functions

;; bug-change-dir : Bug Dir -> Bug
;; Change bug's direction to given one
(define (bug-change-dir b d)
  ; stub
  b)

;; bug-advance : Bug -> Bug
;; Advance bug in its current direction, but not past board boundaries
(define (bug-advance b)
  ; stub
  b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Posn functions

;; posn-advance : Posn Dir -> Posn
;; Advance the posn in given direction, but not past boundaries
(define (posn-advance p d)
  ; stub
  p)

;; posn-advance-left : Posn -> Posn
;; Advance the posn toward left, but not past left boundary
(define (posn-advance-left p)
  ; stub
  p)

;; posn-advance-right : Posn -> Posn
;; Advance the posn toward right, but not past right boundary
(define (posn-advance-right p)
  ; stub
  p)

;; posn-advance-up : Posn -> Posn
;; Advance the posn toward top, but not past top boundary
(define (posn-advance-up p)
  ; stub
  p)

;; posn-advance-down : Posn -> Posn
;; Advance the posn toward bottom, but not past bottom boundary
(define (posn-advance-down p)
  ; stub
  p)

;; posn=? : Posn Posn -> Boolean
;; Are the two posns at the same position?
(define (posn=? p1 p2)
  ; stub
  #false)

;; posn-draw-on : Posn Color Scene -> Scene
;; Draw a colored circled at given posn on scene
(define (posn-draw-on p color scn)
  (place-image (circle (* 1/2 PX/U) "solid" color)
               (+ (* (posn-x p) PX/U) (* 1/2 PX/U))
               (+ (* (posn-y p) PX/U) (* 1/2 PX/U))
               scn))

