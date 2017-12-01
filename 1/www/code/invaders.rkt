;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname invaders-dr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Authors: dvanhorn
; Purpose: Simplified version of Space Invaders

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Game is one of:
;; - Aim
;; - Fire

;; An Aim is (make-aim Invader Base)
(define-struct aim (inv base))
;; Interp: game state with no shot fired

;; A Fire is (make-fire Invader Base Shot)
(define-struct fire (inv base shot))
;; Interp: game state with a shot fired

;; An Invader is a (make-invader Integer Integer Dir)
(define-struct invader (x y dir))
;; Interp: the (x,y) px coordinate of the upper left corner of the invader
;; moving in the direction dir

;; A Dir is one of:
;; - "left"
;; - "right"
;; Interp: direction of movement

;; A Base is an Integer
;; Interp: the px coordinate of the left edge of the base

;; A Shot is (make-posn Integer Integer)
;; Interp: the px coordinate of the shot

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main

(define (game-main i)
  (big-bang (make-aim INV0 i)
            [to-draw game-draw]
            [on-tick game-tock]
            [on-key game-key]
            [stop-when game-over?]))

(define (base-main i)
  (big-bang i
            [to-draw base-draw]
            [on-key base-key]))

(define (invader-main i)
  (big-bang (make-invader i 0 "right")
            [to-draw invader-draw]
            [on-tick invader-tock]
            [stop-when invader-on-bottom?]))

(define (shot-main i)
  (big-bang (make-posn i SCENE-HEIGHT)
            [to-draw shot-draw]
            [on-tick shot-tock]
            [stop-when shot-over-top?]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

(define BASE-WIDTH 100)
(define BASE-HEIGHT 20)
(define BASE-IMAGE (rectangle BASE-WIDTH BASE-HEIGHT "solid" "green"))
(define SCENE-HEIGHT 200)
(define SCENE-WIDTH 500)
(define MT-SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))
(define BASE-SPEED 10)

(define INV-WIDTH 80)
(define INV-HEIGHT 30)
(define INV-IMAGE (rectangle INV-WIDTH INV-HEIGHT "solid" "red"))
(define INV-SPEED 10)
(define INV-MAX-X (- SCENE-WIDTH INV-WIDTH))
(define INV-MAX-Y (- SCENE-HEIGHT INV-HEIGHT))

(define INV-X-OFF (* 1/2 INV-WIDTH))
(define INV-Y-OFF (* 1/2 INV-HEIGHT))

(define SHOT-SIZE 15)
(define SHOT-IMAGE (triangle SHOT-SIZE "solid" "blue"))
(define SHOT-SPEED 8)

(define BASE-X-OFF (* 1/2 BASE-WIDTH))
(define BASE-Y-OFF (- SCENE-HEIGHT (* 1/2 BASE-HEIGHT)))

(define INV0 (make-invader 0 0 "right"))
(define INV1 (make-invader INV-SPEED 0 "right"))
(define INVN (make-invader 0 INV-MAX-Y "right")) ;; on the bottom

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game functions

(define (game-template g)
  (cond [(aim? g) (aim-template g)]
        [(fire? g) (fire-template g)]))

;; game-draw : Game -> Image
;; Draw the game on an empty scene
(define (game-draw g)
  (cond [(aim? g) (aim-draw g)]
        [(fire? g) (fire-draw g)]))

;; game-tock : Game -> Game
;; Advance the game one tick of time
(define (game-tock g)
  (cond [(aim? g) (aim-tock g)]
        [(fire? g) (fire-tock g)]))

;; game-key : Game KeyEvent -> Game
;; Handle left/right arrow keys by moving base, space fires a shot if available
(define (game-key g ke)
  (cond [(aim? g) (aim-key g ke)]
        [(fire? g) (fire-key g ke)]))

;; game-over?
;; Has the invader reached the bottom of the scene in this game?
(define (game-over? g)
  (cond [(aim? g) (aim-over? g)]
        [(fire? g) (fire-over? g)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aim functions

(define (aim-template a)
  (... (invader-template (aim-inv a))
       (base-template (aim-base a))
       ...))

;; aim-draw : Aim -> Image
;; Draws the aim on an empty
(define (aim-draw a)
  (invader-draw-on (aim-inv a)
                   (base-draw (aim-base a))))

;; aim-tock : Aim -> Aim
;; Advance the aim state one tick of time
(check-expect (aim-tock (make-aim INV0 50))
              (make-aim INV1 50))
(define (aim-tock a)
  (make-aim (invader-tock (aim-inv a))
            (aim-base a)))

;; aim-key : Aim KeyEvent -> Aim
;; Handle left/right, space bar key events, firing shot on space
(check-expect (aim-key (make-aim INV0 50) "right")
              (make-aim INV0 (+ 50 BASE-SPEED)))
(check-expect (aim-key (make-aim INV0 BASE-SPEED) "left")
              (make-aim INV0 0))
(check-expect (aim-key (make-aim INV0 0) " ")
              (make-fire INV0 0 (make-posn BASE-X-OFF BASE-Y-OFF)))
(check-expect (aim-key (make-aim INV0 0) "r")
              (make-aim INV0 0))
(define (aim-key a ke)
  (cond [(string=? ke " ")
         (make-fire (aim-inv a) (aim-base a) (base->shot (aim-base a)))]
        [else
         (make-aim (aim-inv a) (base-key (aim-base a) ke))]))

;; aim-over? : Aim -> Boolean
;; Is the invader in the game on the bottom?
(check-expect (aim-over? (make-aim INV0 50)) #false)
(check-expect (aim-over? (make-aim INVN 50)) #true)
(define (aim-over? a)
  (invader-on-bottom? (aim-inv a)))
  
;; Test
(check-expect (aim-draw (make-aim (make-invader 0 0 "left") 100))
              (place-image INV-IMAGE
                           INV-X-OFF
                           INV-Y-OFF
                           (place-image BASE-IMAGE
                                        (+ BASE-X-OFF 100)
                                        BASE-Y-OFF
                                        MT-SCENE)))                            


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fire functions

(define (fire-template f)
  (... (fire-inv f)
       (fire-base f)
       (fire-shot f)
       ...))

;; fire-draw : Fire -> Image
;; Draw the invader, base, and shot on an empty scene
(define (fire-draw f)
  (shot-draw-on (fire-shot f)
                (invader-draw-on (fire-inv f)
                                 (base-draw (fire-base f)))))

;; fire-tock : Fire -> Fire
;; Advance the fire one tick of the clock
(check-expect (fire-tock (make-fire INV0 0 (make-posn (+ INV-WIDTH 10) 20)))
              (make-fire INV1 0 (make-posn (+ INV-WIDTH 10) (- 20 SHOT-SPEED))))
(check-expect (fire-tock (make-fire INV0 0 (make-posn 0 0)))
              (make-aim INV0 0))
(check-expect (fire-tock (make-fire INV0 0 (make-posn (+ INV-WIDTH 10) -1)))
              (make-aim INV1 0))
(define (fire-tock f)
  (cond [(shot-hits-invader? (fire-shot f) (fire-inv f))
         (make-aim INV0 (fire-base f))]
        [(shot-over-top? (fire-shot f))
         (make-aim (invader-tock (fire-inv f)) (fire-base f))]
        [else
         (make-fire (invader-tock (fire-inv f))
                    (fire-base f)
                    (shot-tock (fire-shot f)))]))
  

;; fire-key : Fire KeyEvent -> Fire
;; Handle left/right keys to move the base
(check-expect (fire-key (make-fire INV0 0 (make-posn 10 20)) "right")
              (make-fire INV0 BASE-SPEED (make-posn 10 20)))
(check-expect (fire-key (make-fire INV0 BASE-SPEED (make-posn 10 20)) "left")
              (make-fire INV0 0 (make-posn 10 20)))
(check-expect (fire-key (make-fire INV0 0 (make-posn 10 20)) " ")
              (make-fire INV0 0 (make-posn 10 20)))
(check-expect (fire-key (make-fire INV0 0 (make-posn 10 20)) "a")
              (make-fire INV0 0 (make-posn 10 20)))
(define (fire-key f ke)
  (make-fire (fire-inv f) (base-key (fire-base f) ke) (fire-shot f)))

;; fire-over? : Fire -> Boolean
;; Is the invader in this game on the bottom?
(check-expect (fire-over? (make-fire INV0 0 (make-posn 10 20))) #false)
(check-expect (fire-over? (make-fire INVN 0 (make-posn 10 20))) #true)
(define (fire-over? f)
  (invader-on-bottom? (fire-inv f)))


;; Tests
(check-expect (fire-draw (make-fire (make-invader 0 0 "left")
                                    100
                                    (make-posn 200 20)))
              (place-image SHOT-IMAGE
                           200
                           20               
                           (place-image INV-IMAGE
                                        INV-X-OFF
                                        INV-Y-OFF
                                        (place-image BASE-IMAGE
                                                     (+ BASE-X-OFF 100)
                                                     BASE-Y-OFF
                                                     MT-SCENE))))


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


;; shot-hits-invader? : Shot Invader -> Boolean
;; Does the shot hit the invader?
(check-expect (shot-hits-invader? (make-posn 0 0) INV0) #true)
(check-expect (shot-hits-invader? (make-posn (+ 10 INV-WIDTH) 0) INV0) #false)
(check-expect (shot-hits-invader? (make-posn 0 (+ 10 INV-HEIGHT)) INV0) #false)
(define (shot-hits-invader? s i)
  (and (<= (invader-x i) (posn-x s) (+ (invader-x i) INV-WIDTH))
       (<= (invader-y i) (posn-y s) (+ (invader-y i) INV-HEIGHT))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invader functions

(define (invader-template i)
  (... (invader-x i)
       (invader-y i)
       (dir-template (invader-dir i))
       ...))

;; invader-draw : Invader -> Image
;; Render the invader on an empty scene
(check-expect (invader-draw INV0)
              (place-image INV-IMAGE INV-X-OFF INV-Y-OFF MT-SCENE))
(check-expect (invader-draw (make-invader 0 0 "left"))
              (place-image INV-IMAGE INV-X-OFF INV-Y-OFF MT-SCENE))
(check-expect (invader-draw (make-invader 10 0 "left"))
              (place-image INV-IMAGE (+ 10 INV-X-OFF) INV-Y-OFF MT-SCENE))
(define (invader-draw i)
  (invader-draw-on i MT-SCENE))

;; invader-draw-on : Invader Image -> Image
;; Render the invader on the given scene
(check-expect (invader-draw-on INV0 MT-SCENE)
              (place-image INV-IMAGE INV-X-OFF INV-Y-OFF MT-SCENE))
(define (invader-draw-on i img)  
  (place-image INV-IMAGE
               (+ (invader-x i) INV-X-OFF)
               (+ (invader-y i) INV-Y-OFF)
               img))

;; invader-tock : Invader -> Invader
;; Advance the invader one tick of time
(check-expect (invader-tock INV0) INV1)
(check-expect (invader-tock (make-invader 0 INV-SPEED "left"))
              (make-invader 0 (* 2 INV-SPEED) "right"))
(check-expect (invader-tock (make-invader 0 0 "left"))
              (make-invader 0 INV-SPEED "right"))
(define (invader-tock i)
  (make-invader (+ (invader-x i) (delta-x (invader-dir i) (invader-x i)))
                (+ (invader-y i) (delta-y (invader-dir i) (invader-x i)))
                (dir-tock (invader-dir i) (invader-x i))))

;; invader-on-bottom? : Invader -> Boolean
;; Is the invader on the bottom of the scene?
(check-expect (invader-on-bottom? (make-invader 0 0 "left")) #false)
(check-expect (invader-on-bottom? INVN) #true)
(define (invader-on-bottom? i)
  (= INV-MAX-Y (invader-y i)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direction functions

;; delta-x : Dir Integer -> Integer
;; Compute the change in x position given an x position and direction
(check-expect (delta-x "left" 0) 0)
(check-expect (delta-x "left" INV-SPEED) (- INV-SPEED))
(check-expect (delta-x "right" 0) INV-SPEED)
(check-expect (delta-x "right" INV-MAX-X) 0)
(define (delta-x dir x)
  (cond [(string=? dir "left")
         (cond [(= x 0) 0]
               [else (- INV-SPEED)])]
        [(string=? dir "right")
         (cond [(= x INV-MAX-X) 0]
               [else INV-SPEED])]))

;; delta-y : Dir Integer -> Integer
;; Compute the change in y position given an x position and direction
(check-expect (delta-y "left" 0) INV-SPEED)
(check-expect (delta-y "right" 0) 0)
(check-expect (delta-y "right" INV-MAX-X) INV-SPEED)
(check-expect (delta-y "left" INV-MAX-X) 0)
(define (delta-y dir x)
  (cond [(string=? dir "left")
         (cond [(= x 0) INV-SPEED]
               [else 0])]
        [(string=? dir "right")
         (cond [(= x INV-MAX-X) INV-SPEED]
               [else 0])]))

;; dir-tock : Dir Integer -> Dir
;; Compute the new direction given current direction and x position
(check-expect (dir-tock "left" 0) "right")
(check-expect (dir-tock "right" 0) "right")
(check-expect (dir-tock "right" INV-MAX-X) "left")
(check-expect (dir-tock "left" INV-MAX-X) "left")
(define (dir-tock dir x)
  (cond [(string=? dir "left")
         (cond [(= x 0) "right"]
               [else "left"])]
        [(string=? dir "right")
         (cond [(= x INV-MAX-X) "left"]
               [else "right"])]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base functions

(define (base-template b)
  (... b ...))

;; draw-base : Base -> Image
;; Render a base on an empty scene
(check-expect (base-draw 0)
              (place-image BASE-IMAGE BASE-X-OFF BASE-Y-OFF MT-SCENE))
(check-expect (base-draw 100)
              (place-image BASE-IMAGE (+ 100 BASE-X-OFF) BASE-Y-OFF MT-SCENE))
(define (base-draw b)
  (place-image BASE-IMAGE
               (+ b (* 1/2 BASE-WIDTH))
               (- SCENE-HEIGHT (* 1/2 BASE-HEIGHT))
               MT-SCENE))


;; base-key : Base KeyEvent -> Base
;; Move the base left and right in response to left & right arrow keys
(check-expect (base-key 0 "left") (max 0 (- 0 BASE-SPEED)))
(check-expect (base-key 1 "left") (max 0 (- 1 BASE-SPEED)))
(check-expect (base-key 0 "right")
              (min (- SCENE-WIDTH BASE-WIDTH) (+ 0 BASE-SPEED)))
(check-expect (base-key 5 "right")
              (min (- SCENE-WIDTH BASE-WIDTH) (+ 5 BASE-SPEED)))
(check-expect (base-key (- SCENE-WIDTH BASE-WIDTH) "right")
              (- SCENE-WIDTH BASE-WIDTH))
(check-expect (base-key 0 "a") 0)
(define (base-key b ke)
  (cond [(string=? ke "left")
         (max 0 (- b BASE-SPEED))]
        [(string=? ke "right")
         (min (- SCENE-WIDTH BASE-WIDTH) (+ b BASE-SPEED))]
        [else b]))


;; base->shot : Base -> Shot
;; Compute the location of a shot fired from given base
(check-expect (base->shot 50) (make-posn (+ 50 BASE-X-OFF) BASE-Y-OFF))
(define (base->shot b)
  (make-posn (+ b BASE-X-OFF) BASE-Y-OFF))



