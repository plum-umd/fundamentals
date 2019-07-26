;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname snake-client) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Snake Client

(require 2htdp/universe)
(require 2htdp/image)

;; main : Anything -> Game
;; Launches a game of "Snake" played at given tick rate
;; Example: (main 1/10)
(define (main _)
  (big-bang G0
    [to-draw game-draw]
    [on-key game-send-key]
    [on-receive game-receive]
    [register "127.0.0.1"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A Game is a (make-game Snake Food [Lo Snake])
(define-struct game (snake food others))

;; A Bug is a (make-bug Dir Posn [Lo Posn])
(define-struct snake (dir head tail))

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
(define BKGRD (empty-scene (* WIDTH PX/U) (* HEIGHT PX/U)))

(define S0
  (make-snake "up"
              (make-posn (quotient WIDTH 2)
                         (quotient HEIGHT 2))
              '()))

(define S1
  (make-snake "down"
              (make-posn 0 0)
              (list (make-posn 0 1)
                    (make-posn 0 2))))
   
(define G0 ; an initial game state
  (make-game S0
             (make-posn 0 0)
             '()))

(define G0-msg ; msg representation of G0, for testing
  (list (list "up"
              (list (quotient WIDTH 2)
                    (quotient HEIGHT 2))
              '())
        (list 0 0)
        '()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game functions

;; game-send-key : Game KeyEvent -> (make-package Game Dir) or Game
;; Handle a key event in this game
(check-expect (game-send-key G0 "left") (make-package G0 "left"))
(check-expect (game-send-key G0 "right") (make-package G0 "right"))
(check-expect (game-send-key G0 "up") (make-package G0 "up"))
(check-expect (game-send-key G0 "down") (make-package G0 "down"))
(check-expect (game-send-key G0 "\r") G0)
(define (game-send-key g ke)
  (cond [(or (key=? ke "left")
             (key=? ke "right")
             (key=? ke "up")
             (key=? ke "down"))
         (make-package g ke)]
        [else g]))

;; game-receive : Game S-Expression -> Game
;; Install game state from server
(check-expect (game-receive G0 G0-msg) (msg-to-game G0-msg))
(define (game-receive g msg)
  (msg-to-game msg))

;; game-draw : Game -> Image
;; Render the game as a scene
(check-expect (game-draw G0)
              (snake-draw-on (game-snake G0)
                             (food-draw-on (game-food G0) BKGRD)))
(define (game-draw g)
  (snake-draw-on (game-snake g)
                 (others-draw-on (game-others g)
                                 (food-draw-on (game-food g)
                                               BKGRD))))


;; food-draw-on : Posn Image -> Image
;; Draw food on the image
(check-expect (food-draw-on (make-posn 0 0) BKGRD)
              (posn-draw-on (make-posn 0 0) "solid" "green" BKGRD))
(define (food-draw-on p img)
  (posn-draw-on p "solid" "green" img))

;; others-draw-on : [Lo Snake] Image -> Image
;; Draw "other" snakes on the image
(check-expect (others-draw-on '() BKGRD) BKGRD)
(check-expect (others-draw-on (list S0 S1) BKGRD)
              (other-snake-draw-on S0 (other-snake-draw-on S1 BKGRD)))
(define (others-draw-on ss img)
  (foldr other-snake-draw-on img ss))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snake functions

;; snake-draw-on : Snake Image -> Image
;; Draw the snake on the scene
(check-expect (snake-draw-on (make-snake "up" (make-posn 0 0) '()) BKGRD)
              (posn-draw-on (make-posn 0 0) "solid" "red" BKGRD))
(check-expect (snake-draw-on (make-snake "up" (make-posn 0 0)
                                         (list (make-posn 0 1)))
                             BKGRD)
              (posn-draw-on (make-posn 0 0) "solid" "red"
                            (posn-draw-on (make-posn 0 1) "solid" "red"
                                          BKGRD)))
(define (snake-draw-on s scn)
  (local [(define (posn-draw-red-on p i)
            (posn-draw-on p "solid" "red" i))]            
    (posn-draw-on (snake-head s) "solid" "red"
                  (foldr posn-draw-red-on
                         scn
                         (snake-tail s)))))

;; other-snake-draw-on : Snake Image -> Image
;; Draw the snake on the scene
(check-expect (other-snake-draw-on (make-snake "up" (make-posn 0 0) '()) BKGRD)
              (posn-draw-on (make-posn 0 0) "outline" "red" BKGRD))
(check-expect (other-snake-draw-on (make-snake "up" (make-posn 0 0)
                                               (list (make-posn 0 1)))
                             BKGRD)
              (posn-draw-on (make-posn 0 0) "outline" "red"
                            (posn-draw-on (make-posn 0 1) "outline" "red"
                                          BKGRD)))
(define (other-snake-draw-on s scn)
  (local [(define (posn-draw-red-on p i)
            (posn-draw-on p "outline" "red" i))]            
    (posn-draw-on (snake-head s) "outline" "red"
                  (foldr posn-draw-red-on
                         scn
                         (snake-tail s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Posn functions

;; posn-draw-on : Posn Color Scene -> Scene
;; Draw a colored circled at given posn on scene
(define (posn-draw-on p mode color scn)
  (place-image (circle (* 1/2 PX/U) mode color)
               (+ (* (posn-x p) PX/U) (* 1/2 PX/U))
               (+ (* (posn-y p) PX/U) (* 1/2 PX/U))
               scn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; S-Expression conversion

;; S-Expression -> Game
(check-expect (msg-to-game (list (list "left" (list 3 4) (list (list 3 5) (list 3 6)))
                                 (list 8 8)
                                 '()))
              (make-game (make-snake "left"
                                     (make-posn 3 4)
                                     (list (make-posn 3 5)
                                           (make-posn 3 6)))
                         (make-posn 8 8)
                         '()))
(define (msg-to-game msg)
  (make-game (msg-to-snake (first msg))
             (msg-to-posn (second msg))
             (map msg-to-snake (third msg))))

;; S-Expression -> Snake
(check-expect (msg-to-snake (list "left" (list 3 4) (list (list 3 5) (list 3 6))))
              (make-snake "left"
                          (make-posn 3 4)
                          (list (make-posn 3 5)
                                (make-posn 3 6))))
(define (msg-to-snake msg)
  (make-snake (first msg)
              (msg-to-posn (second msg))
              (map msg-to-posn (third msg))))

;; S-Expression -> Posn
(check-expect (msg-to-posn (list 3 4)) (make-posn 3 4))
(define (msg-to-posn msg)
  (make-posn (first msg)
             (second msg)))

