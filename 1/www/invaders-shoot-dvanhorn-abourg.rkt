;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname invader-shots-dvanhorn-abourg) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Authors: dvanhorn, abourg
; Purpose: Space Invaders w/ 1 row of Invaders

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A Game is a (make-game InvaderRow Base Shots Shots)
(define-struct game (row base shots-up shots-down))
; Interp: game state with an arbitrary number of shots fired up & down.

;; An LoI (list of Invaders) is one of:
;; - '()
;; - (cons Invader LoI)
;; Interp: a collection of invaders

;; An InvaderRow is a (make-row Invader Invader LoI)
(define-struct row (left right invaders))
;; Interp: a collection of invaders with a "ghost" left- & right-most invader

;; An Invader is a (make-invader Integer Integer Dir Timer)
(define-struct invader (x y dir timer))
;; Interp: the (x,y) px coordinate of the upper left corner of the invader
;; moving in the direction dir, timer counting #ticks until next shot

;; A Timer is a (make-posn Natural Natural)
;; Interp: x = #ticks until timer goes off, y = period of timer

;; A Dir is one of:
;; - "left"
;; - "right"
;; Interp: direction of movement

;; A Base is an (make-base Integer Natural)
(define-struct base (x lives))
;; Interp: the px coordinate of the left edge of the base, # lives remaining

;; A Shots is one of:
;; - '()
;; - (cons Shot Shots)
;; Interp: a list of 0 or more shots in the game

;; A Shot is (make-posn Integer Integer)
;; Interp: the px coordinate of the shot

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main

(define (game-main i)
  (big-bang (make-game R0 (make-base i LIVES0) '() '())
            [to-draw game-draw]
            [on-tick game-tock]
            [on-key game-key]
            [stop-when game-over?]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

(define BASE-WIDTH 60)
(define BASE-HEIGHT 20)
(define BASE-IMAGE (rectangle BASE-WIDTH BASE-HEIGHT "solid" "green"))
(define SCN-HEIGHT 200)
(define SCN-WIDTH 500)
(define MT-SCN (empty-scene SCN-WIDTH SCN-HEIGHT))
(define A-SCN
  (overlay (text "Austin!" 50 "red") MT-SCN))
               
(define BASE-SPEED 10)

(define INV-WIDTH 40)
(define INV-HEIGHT 20)
(define INV-IMAGE (rectangle INV-WIDTH INV-HEIGHT "solid" "red"))
(define INV-SPEED 10)
(define INV-MAX-X (- SCN-WIDTH INV-WIDTH))
(define INV-MAX-Y (- SCN-HEIGHT INV-HEIGHT))

(define INV-X-OFF (* 1/2 INV-WIDTH))
(define INV-Y-OFF (* 1/2 INV-HEIGHT))

(define SHOT-SIZE 15)
(define SHOT-UP-IMAGE (triangle SHOT-SIZE "solid" "blue"))
(define SHOT-DOWN-IMAGE (rotate 180 SHOT-UP-IMAGE))
(define SHOT-SPEED 8)
(define S0 (make-posn 0 0))

(define BASE-X-OFF (* 1/2 BASE-WIDTH))
(define BASE-Y-OFF (- SCN-HEIGHT (* 1/2 BASE-HEIGHT)))
(define BASE-MAX-X (- SCN-WIDTH BASE-WIDTH))

(define TIMER0 (make-posn 10 40))
(define TIMER1 (make-posn 9 40))
(define TIMERN (make-posn 0 40))

(define INV0  (make-invader 0 0 "right" TIMER0))
(define INV0+ (make-invader 0 0 "right" TIMER1))
(define INV1- (make-invader INV-SPEED 0 "right" TIMER0))
(define INV1  (make-invader INV-SPEED 0 "right" TIMER1))
(define INV0L (make-invader 0 0 "left" TIMER1))
(define INV1L (make-invader INV-SPEED 0 "left" TIMER1))
(define INVR  (make-invader INV-MAX-X 0 "right" TIMERN)) ;; on right
(define INVD  (make-invader INV-MAX-X INV-SPEED "left" TIMERN)) ;; down INVR
(define INVN  (make-invader 0 INV-MAX-Y "right" TIMER0)) ;; on the bottom

(define LIVES0 3)

(define B0 (make-base 0 LIVES0))
(define BASE1 (make-base 1 LIVES0))
(define BASE5 (make-base 5 LIVES0))
(define B100 (make-base 100 LIVES0))
(define BASEN (make-base (- SCN-WIDTH BASE-WIDTH) LIVES0))

(define INV-GAP 10) ;; px between invaders
(define INV-WIDTH+GAP (+ INV-WIDTH INV-GAP))
(define D0 "right")
;; Add invaders here:
(define INVS0
  (list (make-invader (* 0 INV-WIDTH+GAP) 0 D0 (make-posn 30 40))
        (make-invader (* 1 INV-WIDTH+GAP) 0 D0 (make-posn 20 50))
        (make-invader (* 2 INV-WIDTH+GAP) 0 D0 (make-posn 10 60))
        (make-invader (* 3 INV-WIDTH+GAP) 0 D0 (make-posn 40 50))))

(define R0
  (make-row (first INVS0)
            (first (reverse INVS0)) ; last element
            INVS0))

;; t-row : Invader -> Row
;; Turn an invader into a singleton row for testing
(define (t-row i) (make-row i i (list i)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game functions

(define (game-template g)
  (... (invader-template (game-inv g))
       (base-template (game-base g))
       (shots-template (game-shots-up g))
       (shots-template (game-shots-down g))
       ...))

;; game-tock : Game -> Game
;; Advance the game one tick of time
(check-expect (game-tock (make-game R0 B0 '() '()))
              (make-game (row-tock R0) B0 '() '()))
(check-expect (game-tock (make-game (t-row INVR) B0 '() '()))
              (make-game (row-tock (t-row INVR)) B0 '()
                         (list (invader->shot INVR))))
(check-expect (game-tock (make-game R0 B0 '() (list (invader->shot INVR))))
              (make-game (row-tock R0) B0 '()
                         (list (shot-down-tock (invader->shot INVR)))))
(check-expect (game-tock (make-game R0 B0 (list (base->shot B0)) '()))
              (make-game (row-tock R0) B0
                         (list (shot-up-tock (base->shot B0))) '()))
(check-expect (game-tock (make-game R0 B0 '() (list (make-posn 0 SCN-HEIGHT))))
              (make-game R0 (base-kill B0) '() '()))
(define (game-tock g)
  (if (any-shots-hit-base? (game-shots-down g) (game-base g))
      (game-kill g)
      (game-continue g)))

;; game-kill : Game -> Game
;; Kill the base and reset
(check-expect (game-kill (make-game R0 B0 '() (list (make-posn 0 SCN-HEIGHT))))
              (make-game R0 (base-kill B0) '() '()))
(define (game-kill g)
  (make-game (game-row g)
             (base-kill (game-base g))
             '()
             '()))

;; game-continue : Game -> Game
;; Continue the game (base not hit)
(check-expect (game-continue (make-game R0 B0 (list (base->shot B0)) (list S0)))
              (make-game (row-tock R0) B0
                         (list (shot-up-tock (base->shot B0)))
                         (list (shot-down-tock S0))))
(check-expect (game-continue (make-game (t-row INVR) B0 '() (list S0)))
              (make-game (row-tock (t-row INVR)) B0 '()
                         (list (invader->shot INVR) (shot-down-tock S0))))
(define (game-continue g)
  (make-game (row-tock&remove (game-row g) (game-shots-up g))
             (game-base g)
             (shots-up-tock&remove (game-shots-up g) (game-row g))
             (shots-down-shoot&tock (game-shots-down g) (game-row g))))

;; game-key : Game KeyEvent -> Game
;; Handle left/right arrow keys by moving base, space fires a shot if available
(check-expect (game-key (make-game R0 B0 '() '()) " ")
              (make-game R0 B0 (cons (base->shot B0) '()) '()))
(check-expect (game-key (make-game R0 B0 (list (base->shot B100)) '()) " ")
              (make-game R0 B0 (list (base->shot B0) (base->shot B100)) '()))
(check-expect (game-key (make-game R0 B0 (list (base->shot B100)) '()) "right")
              (make-game R0 (base-key B0 "right") (list (base->shot B100)) '()))
(check-expect (game-key (make-game R0 B0 (list (base->shot B100)) '()) "z")
              (make-game R0 (base-key B0 "z") (list (base->shot B100)) '()))
(define (game-key g ke)
  (cond [(string=? ke " ")
         (make-game (game-row g)
                    (game-base g)
                    (cons (base->shot (game-base g)) (game-shots-up g))
                    (game-shots-down g))]
        [else
         (make-game (game-row g)
                    (base-key (game-base g) ke)
                    (game-shots-up g)
                    (game-shots-down g))]))

;; game-over? : Game -> Boolean
;; Is the base out of lives?
(check-expect (game-over? (make-game R0 (make-base 50 3) '() '())) #false)
(check-expect (game-over? (make-game R0 (make-base 50 0) '() '())) #true)
(define (game-over? g)
  (zero? (base-lives (game-base g))))

;; game-draw : Game -> Image
;; Draw the game on an empty scene
(check-expect
 (game-draw (make-game R0 B0
                       (list (base->shot B100))
                       (list (invader->shot INVR))))
 (shot-up-draw-on (base->shot B100)
                  (shot-down-draw-on (invader->shot INVR)
                                     (row-draw-on R0 (base-draw B0)))))
(define (game-draw g)
  (shots-up-draw-on
   (game-shots-up g)
   (shots-down-draw-on
    (game-shots-down g)
    (row-draw-on
     (game-row g)
     (base-draw (game-base g))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shots functions

(define (shots-template shots)
  (cond [(empty? shots) ...]
        [(cons? shots)
         (... (shot-template (first shots)) 
              ...
              (shots-template (rest shots))
              ...)]))

;; shots-up-tock&remove : Shots Row -> Shots
;; Remove any shots hitting row, tock upward
(check-expect (shots-up-tock&remove (list S0) (t-row INVR)) '())
(define (shots-up-tock&remove shots r)
  (shots-up-tock (shots-remove-hit shots r)))

;; shots-down-shoot&tock : Shots Row -> Shots
;; Tock downard, add any shots from ready invaders in row
(check-expect (shots-down-shoot&tock (list S0) (t-row INVR))
              (list (invader->shot INVR) (shot-down-tock S0)))
(define (shots-down-shoot&tock shots r)
  (row-shoot r (shots-down-tock shots)))
;; shots-up-tock : Shots -> Shots
;; Advances each shot in the list upwards, eliminates all over the top shots
(check-expect (shots-up-tock '()) '())
(check-expect (shots-up-tock (list (base->shot B0) S0))
              (list (shot-up-tock (base->shot B0))))
(check-expect (shots-up-tock (list (make-posn INV-WIDTH 0))) '())
(define (shots-up-tock shots)
  (remove-all-over-top (shots-up-march shots)))

;; shots-down-tock : Shots -> Shots
;; Advances each shot in the list downwards, eliminates all below the bot shots
(check-expect (shots-down-tock '()) '())
(check-expect (shots-down-tock (list (invader->shot INVR)))
              (list (shot-down-tock (invader->shot INVR))))
(define (shots-down-tock shots)
  (remove-all-below-bot (shots-down-march shots)))

;; shots-up-march : Shots -> Shots
;; Advance each shot upward
(check-expect (shots-up-march '()) '())
(check-expect (shots-up-march (list (base->shot B0)))
              (list (shot-up-tock (base->shot B0))))
(check-expect (shots-up-march (list (make-posn INV-WIDTH 0)))
              (list (shot-up-tock (make-posn INV-WIDTH 0))))
(define (shots-up-march shots)
  (cond [(empty? shots) '()]
        [(cons? shots)
         (cons (shot-up-tock (first shots)) 
               (shots-up-march (rest shots)))]))

;; shots-down-march : Shots -> Shots
;; Advance each shot downward
(check-expect (shots-down-march '()) '())
(check-expect (shots-down-march (list (invader->shot INVR)))
              (list (shot-down-tock (invader->shot INVR))))
(check-expect (shots-down-march (list (make-posn INV-WIDTH 0)))
              (list (shot-down-tock (make-posn INV-WIDTH 0))))
(define (shots-down-march shots)
  (cond [(empty? shots) '()]
        [(cons? shots)
         (cons (shot-down-tock (first shots)) 
               (shots-down-march (rest shots)))]))

;; remove-all-over-top : Shots -> Shots
;; Removes all shots that have gone above the top of the screen
(check-expect (remove-all-over-top '()) '())
(check-expect (remove-all-over-top (list (make-posn INV-WIDTH INV-HEIGHT)))
              (list (make-posn INV-WIDTH INV-HEIGHT)))
(check-expect (remove-all-over-top (list (make-posn INV-WIDTH (- INV-HEIGHT))
                                         (make-posn INV-WIDTH INV-HEIGHT)))
              (list (make-posn INV-WIDTH INV-HEIGHT)))
(define (remove-all-over-top shots)
  (cond [(empty? shots) '()]
        [(cons? shots)
         (cond [(shot-over-top? (first shots))
                (remove-all-over-top (rest shots))]
               [else
                (cons (first shots)
                      (remove-all-over-top (rest shots)))])]))

;; remove-all-below-bot : Shots -> Shots
;; Removes all shots that have gone below the bot of the screen
(check-expect (remove-all-below-bot '()) '())
(check-expect (remove-all-below-bot (list (base->shot B0)))
              (list (base->shot B0)))
(check-expect (remove-all-below-bot (list (make-posn 0 (+ 1 SCN-HEIGHT))))
              '())              
(define (remove-all-below-bot shots)
  (cond [(empty? shots) '()]
        [(cons? shots)
         (cond [(shot-below-bot? (first shots))
                (remove-all-below-bot (rest shots))]
               [else
                (cons (first shots)
                      (remove-all-below-bot (rest shots)))])]))

;; shots-remove-hit : Shots Row -> Shots
;; Remove any shots that hit an invader
(check-expect (shots-remove-hit '() R0) '())
(check-expect (shots-remove-hit (list S0) (t-row INV0)) '())
(check-expect (shots-remove-hit (list S0) (t-row INVR)) (list S0))
(define (shots-remove-hit shots r)
  (cond [(empty? shots) '()]
        [(cons? shots)
         (cond [(shot-hits-row? (first shots) r)
                (shots-remove-hit (rest shots) r)]
               [else
                (cons (first shots)
                      (shots-remove-hit (rest shots) r))])]))

;; any-shots-hit-invader? : Shots Invader -> Boolean
;; Do any of the shots hit the invader?
(check-expect (any-shots-hit-invader? '() INV0) #false)
(check-expect (any-shots-hit-invader? (list S0) INV0) #true)
(check-expect (any-shots-hit-invader? (list (make-posn (* 2 INV-WIDTH)
                                                       (* 2 INV-HEIGHT))
                                            (make-posn (* 1/2 INV-WIDTH)
                                                       (* 3 INV-HEIGHT)))
                                      INV0)
              #false)
(define (any-shots-hit-invader? shots inv)
  (cond [(empty? shots) #false]
        [(cons? shots)
         (or (shot-hits-invader? (first shots) inv)
             (any-shots-hit-invader? (rest shots) inv))]))

;; any-shots-hit-base? : Shots Base -> Boolean
;; Do any of the shots hit the base?
(check-expect (any-shots-hit-base? '() B0) #false)
(check-expect (any-shots-hit-base? (list (make-posn 0 SCN-HEIGHT)) B0) #true)
(check-expect (any-shots-hit-base? (list (make-posn BASE-MAX-X 0)) B0) #false)
(define (any-shots-hit-base? shots base)
  (cond [(empty? shots) #false]
        [(cons? shots)
         (or (shot-hits-base? (first shots) base)
             (any-shots-hit-base? (rest shots) base))]))

;; shots-up-draw-on : Shots Image -> Image
;; Draws all of the shots upward on to the given image
(check-expect (shots-up-draw-on '() A-SCN) A-SCN)
(check-expect (shots-up-draw-on (list (base->shot B0)) A-SCN)
              (shot-up-draw-on (base->shot B0) A-SCN))
(define (shots-up-draw-on shots img)
  (cond [(empty? shots) img]
        [(cons? shots)
         (shot-up-draw-on (first shots)
                          (shots-up-draw-on (rest shots) img))]))

;; shots-down-draw-on : Shots Image -> Image
;; Draws all of the shots downward on the given image
(check-expect (shots-down-draw-on '() A-SCN) A-SCN)
(check-expect (shots-down-draw-on (list (invader->shot INVR)) A-SCN)
              (shot-down-draw-on (invader->shot INVR) A-SCN))
(define (shots-down-draw-on shots img)
  (cond [(empty? shots) img]
        [(cons? shots)
         (shot-down-draw-on (first shots)
                            (shots-down-draw-on (rest shots) img))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shot functions

(define (shot-template s)
  (... (posn-x s)
       (posn-y s) ...))

;; shot-up-draw-on : Shot Image -> Image
;; Draw shot on given image
(check-expect (shot-up-draw-on (make-posn 100 200) A-SCN)
              (place-image SHOT-UP-IMAGE 100 200 A-SCN))
(define (shot-up-draw-on s img)
  (place-image SHOT-UP-IMAGE (posn-x s) (posn-y s) img))

;; shot-down-draw-on : Shot Image -> Image
;; Draw shot on given image
(check-expect (shot-down-draw-on (make-posn 100 200) A-SCN)
              (place-image SHOT-DOWN-IMAGE 100 200 A-SCN))
(define (shot-down-draw-on s img)
  (place-image SHOT-DOWN-IMAGE (posn-x s) (posn-y s) img))

;; shot-up-tock : Shot -> Shot
;; Advance the shot upward one tick of time
(check-expect (shot-up-tock (make-posn 100 200))
              (make-posn 100 (- 200 SHOT-SPEED)))
(define (shot-up-tock s)
  (make-posn (posn-x s)
             (- (posn-y s) SHOT-SPEED)))

;; shot-down-tock : Shot -> Shot
;; Advance the shot downward one tick of time
(check-expect (shot-down-tock (make-posn 100 200))
              (make-posn 100 (+ 200 SHOT-SPEED)))
(define (shot-down-tock s)
  (make-posn (posn-x s)
             (+ (posn-y s) SHOT-SPEED)))

;; shot-over-top? : Shot -> Boolean
;; Has the shot gone over the top of the scene?
(check-expect (shot-over-top? (make-posn 100 200)) #false)
(check-expect (shot-over-top? (make-posn 100 0)) #false)
(check-expect (shot-over-top? (make-posn 100 -1)) #true)
(define (shot-over-top? s)
  (< (posn-y s) 0))

;; shot-below-bot? : Shot -> Boolean
;; Has the shot gone past the bottom of the scene?
(check-expect (shot-below-bot? (make-posn 100 200)) #false)
(check-expect (shot-below-bot? (make-posn 100 SCN-HEIGHT)) #false)
(check-expect (shot-below-bot? (make-posn 100 (add1 SCN-HEIGHT))) #true)
(define (shot-below-bot? s)
  (> (posn-y s) SCN-HEIGHT))

;; shot-hits-invader? : Shot Invader -> Boolean
;; Does the shot hit the invader?
(check-expect (shot-hits-invader? S0 INV0) #true)
(check-expect (shot-hits-invader? (make-posn (+ 10 INV-WIDTH) 0) INV0) #false)
(check-expect (shot-hits-invader? (make-posn 0 (+ 10 INV-HEIGHT)) INV0) #false)
(define (shot-hits-invader? s i)
  (and (<= (invader-x i) (posn-x s) (+ (invader-x i) INV-WIDTH))
       (<= (invader-y i) (posn-y s) (+ (invader-y i) INV-HEIGHT))))

;; shot-hits-base? : Shot Base -> Boolean
;; Does the shot hit the base?
(check-expect (shot-hits-base? S0 B0) #false)
(check-expect (shot-hits-base? (make-posn 0 SCN-HEIGHT ) B0) #true)
(check-expect (shot-hits-base? (make-posn (add1 BASE-WIDTH) SCN-HEIGHT) B0)
              #false)
(define (shot-hits-base? s b)
  (and (<= (base-x b) (posn-x s) (+ (base-x b) BASE-WIDTH))
       (<= (- SCN-HEIGHT BASE-HEIGHT) (posn-y s) SCN-HEIGHT)))

;; shot-hits-row? : Shot Row -> Boolean
(define (shot-hits-row? s r)
  (invaders-hit-shot? (row-invaders r) s))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Row functions

;; row-tock&remove : Row Shots -> Row
;; Remove any shot invaders, tock
(define (row-tock&remove r shots)
  (row-tock (row-remove-shot r shots)))

;; row-remove-shot : Row Shots -> Row
;; Remove all the invaders hit by shots
(check-expect (row-remove-shot R0 '()) R0)
(check-expect (row-remove-shot (t-row INV0) (list S0))
              (make-row INV0 INV0 '()))
(define (row-remove-shot r shots)
  (make-row (row-left r)
            (row-right r)
            (invaders-remove-shot (row-invaders r) shots)))

;; row-tock : Row -> Row
;; Advance the invaders, countdown the timers
(check-expect (row-tock (t-row INV0)) (row-countdown (row-march (t-row INV0))))
(define (row-tock r)
  (row-countdown (row-march r)))

;; row-march : Row -> Row
;; Advance the invaders
(check-expect (row-march (t-row INV0)) (row-move (t-row INV0)))
(check-expect (row-march (t-row INVR)) (row-flip&down (t-row INVR)))
(define (row-march r)
  (if (row-hit-boundary? r)
      (row-flip&down r)
      (row-move r)))

;; row-countdown : Row -> Row
;; Countdown the timers
(check-expect (row-countdown (t-row INV0)) (t-row (invader-countdown INV0)))
(define (row-countdown r)
  (make-row (invader-countdown (row-left r))
            (invader-countdown (row-right r))
            (invaders-countdown (row-invaders r))))

;; row-hit-boundary? : Row -> Boolean
;; Does the row hit the left or right boundary?
(check-expect (row-hit-boundary? (t-row INV0)) #false)
(check-expect (row-hit-boundary? (t-row INVR)) #true)
(define (row-hit-boundary? r)
  (or (invader-hit-left? (row-left r))
      (invader-hit-right? (row-right r))))

;; row-move : Row -> Row
;; Move the row in its current direction
(check-expect (row-move (t-row INV0)) (t-row (invader-move INV0)))
(define (row-move r)
  (make-row (invader-move (row-left r))
            (invader-move (row-right r))
            (invaders-move (row-invaders r))))

;; row-flip&down : Row -> Row
;; Move the row down and flip its direction
(check-expect (row-flip&down (t-row INVR)) (t-row (invader-flip&down INVR)))
(define (row-flip&down r)
  (make-row (invader-flip&down (row-left r))
            (invader-flip&down (row-right r))
            (invaders-flip&down (row-invaders r))))

;; row-shoot : Row Shots -> Shots
;; Add shots for each invader that is ready to shoot
(check-expect (row-shoot (t-row INV0) '()) (invader-shoot INV0 '()))
(define (row-shoot r s)
  (invaders-shoot (row-invaders r) s))

;; row-draw-on : Row Image -> Image
;; Draw the row on the image
(check-expect (row-draw-on (t-row INV0) A-SCN) (invader-draw-on INV0 A-SCN))
(define (row-draw-on r img)
  (invaders-draw-on (row-invaders r) img))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invaders functions

(define (loi-template is)
  (cond [(empty? is) ...]
        [(cons? is)
         (... (invader-template (first is))
              ...
              (loi-template (rest is))
              ...)]))

;; invaders-move : LoI -> LoI
;; Move all the invaders in their current direction
(check-expect (invaders-move '()) '())
(check-expect (invaders-move (list INV0)) (list (invader-move INV0)))
(define (invaders-move is)
  (cond [(empty? is) '()]
        [(cons? is)
         (cons (invader-move (first is))
               (invaders-move (rest is)))]))

;; invaders-flip&down : LoI -> LoI
;; Move all the invaders down and flip their direction
(check-expect (invaders-flip&down '()) '())
(check-expect (invaders-flip&down (list INVR)) (list (invader-flip&down INVR)))
(define (invaders-flip&down is)
  (cond [(empty? is) '()]
        [(cons? is)
         (cons (invader-flip&down (first is))
               (invaders-flip&down (rest is)))]))

;; invaders-shoot : LoI Shots -> Shots
;; Add shots for any invaders ready to shoot
(check-expect (invaders-shoot '() (list S0)) (list S0))
(check-expect (invaders-shoot (list INV0) (list S0))
              (invader-shoot INV0 (list S0)))
(define (invaders-shoot is s)
  (cond [(empty? is) s]
        [(cons? is)
         (invader-shoot (first is)
                        (invaders-shoot (rest is) s))]))

;; invaders-countdown : LoI -> LoI
;; Countdown all the timers
(check-expect (invaders-countdown '()) '())
(check-expect (invaders-countdown (list INV0)) (list (invader-countdown INV0)))
(define (invaders-countdown is)
  (cond [(empty? is) is]
        [(cons? is)
         (cons (invader-countdown (first is))
               (invaders-countdown (rest is)))]))

;; invaders-remove-shot : LoI Shots -> LoI
;; Remove all shot invaders
(check-expect (invaders-remove-shot '() '()) '())
(check-expect (invaders-remove-shot (list INV0) '()) (list INV0))
(check-expect (invaders-remove-shot (list INV0 INVR) (list S0))
              (list INVR))
(define (invaders-remove-shot is shots)
  (cond [(empty? is) '()]
        [(cons? is)
         (cond [(any-shots-hit-invader? shots (first is))
                (invaders-remove-shot (rest is) shots)]
               [else
                (cons (first is)
                      (invaders-remove-shot (rest is) shots))])]))

;; invaders-hit-shot? : LoI Shot -> Boolean
;; Are any of the invaders hit by the shot?
(check-expect (invaders-hit-shot? '() S0) #false)
(check-expect (invaders-hit-shot? (list INV0) S0) #true)
(check-expect (invaders-hit-shot? (list INVR) S0) #false)
(define (invaders-hit-shot? is shot)
  (cond [(empty? is) #false]
        [(cons? is)
         (or (shot-hits-invader? shot (first is))
             (invaders-hit-shot? (rest is) shot))]))

;; invaders-draw-on : LoI Image -> LoI
;; Draw all the invaders on the image
(check-expect (invaders-draw-on '() A-SCN) A-SCN)
(check-expect (invaders-draw-on (list INV0) A-SCN)
              (invader-draw-on INV0 A-SCN))
(define (invaders-draw-on is img)
  (cond [(empty? is) img]
        [(cons? is)
         (invader-draw-on (first is)
                          (invaders-draw-on (rest is) img))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invader functions

(define (invader-template i)
  (... (invader-x i)
       (invader-y i)
       (dir-template (invader-dir i))
       (timer-template (invader-timer i))
       ...))

;; invader-move : Invader -> Invader
;; Move invader left or right, unconditionally
(check-expect (invader-move INV0) INV1-)
(check-expect (invader-move INV1L) INV0L)
(define (invader-move i)
  (make-invader (+ (invader-x i)
                   (if (dir-left? (invader-dir i))
                       (- INV-SPEED)
                       INV-SPEED))
                (invader-y i)
                (invader-dir i)
                (invader-timer i)))

;; invader-flip&down : Invader -> Invader
;; Move invader down, unconditionally, and flip
(check-expect (invader-flip&down INVR) INVD)
(define (invader-flip&down i)
  (make-invader (invader-x i)
                (+ (invader-y i) INV-SPEED)
                (dir-flip (invader-dir i))
                (invader-timer i)))

;; invader-hit-left? : Invader -> Boolean
;; Is the invader moving left & at the left boundary?
(check-expect (invader-hit-left? INV0) #false)
(check-expect (invader-hit-left? INVR) #false)
(check-expect (invader-hit-left? INV0L) #true)
(define (invader-hit-left? i)
  (and (dir-left? (invader-dir i))
       (= 0 (invader-x i))))

;; invader-hit-right? : Invader -> Boolean
;; Is the invader moving right & at the right boundary?
(check-expect (invader-hit-right? INV0) #false)
(check-expect (invader-hit-right? INVR) #true)
(check-expect (invader-hit-right? INVD) #false)
(define (invader-hit-right? i)
  (and (dir-right? (invader-dir i))
       (= INV-MAX-X (invader-x i))))

;; invader-draw-on : Invader Image -> Image
;; Render the invader on the given scene
(check-expect (invader-draw-on INV0 A-SCN)
              (place-image INV-IMAGE INV-X-OFF INV-Y-OFF A-SCN))
(define (invader-draw-on i img)  
  (place-image INV-IMAGE
               (+ (invader-x i) INV-X-OFF)
               (+ (invader-y i) INV-Y-OFF)
               img))

;; invader-countdown : Invader -> Invader
;; Advance the invader's timer one tick
(check-expect (invader-countdown INV0) INV0+)
(define (invader-countdown i)
  (make-invader (invader-x i)
                (invader-y i)
                (invader-dir i)
                (timer-tock (invader-timer i))))

;; invader-on-bottom? : Invader -> Boolean
;; Is the invader on the bottom of the scene?
(check-expect (invader-on-bottom? INV0) #false)
(check-expect (invader-on-bottom? INVN) #true)
(define (invader-on-bottom? i)
  (= INV-MAX-Y (invader-y i)))

;; invader-shoot : Invader Shots -> Shots
;; Add a shot if the invader is ready to shoot
(check-expect (invader-shoot INVR (list (make-posn 20 20)))
              (list (invader->shot INVR) (make-posn 20 20)))
(check-expect (invader-shoot INV0 (list (make-posn 20 20)))
              (list (make-posn 20 20)))
(define (invader-shoot i s)
  (cond [(timer-ready? (invader-timer i))
         (cons (invader->shot i) s)]
        [else s]))

;; invader->shot : Invader -> Shot
;; Convert the invader into a shot
(check-expect (invader->shot INVR)
              (make-posn (+ (invader-x INVR) INV-X-OFF)
                         (+ (invader-y INVR) INV-HEIGHT)))
(define (invader->shot i)
  (make-posn (+ (invader-x i) INV-X-OFF) (+ (invader-y i) INV-HEIGHT)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direction functions

(define (dir-template d)
  (cond [(string=? d "left") ...]
        [(string=? d "right") ...]))

;; dir-flip : Dir -> Dir
;; Flip the direction
(check-expect (dir-flip "left") "right")
(check-expect (dir-flip "right") "left")
(define (dir-flip d)
  (cond [(string=? "left" d) "right"]
        [(string=? "right" d) "left"]))

;; dir-left? : Dir -> Boolean
;; Is the dir left?
(check-expect (dir-left? "left") #true)
(check-expect (dir-left? "right") #false)
(define (dir-left? d)
  (string=? "left" d))

;; dir-right? : Dir -> Boolean
;; Is the dir right?
(check-expect (dir-right? "left") #false)
(check-expect (dir-right? "right") #true)
(define (dir-right? d)
  (string=? "right" d))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base functions

(define (base-template b)
  (... (base-x b) ... (base-lives b) ...))

;; base-kill : Base -> Base
;; Decrement the lives of the base, reset to left side
(check-expect (base-kill (make-base 90 3)) (make-base 0 2))
(define (base-kill b)
  (make-base 0 (sub1 (base-lives b))))

;; draw-base : Base -> Image
;; Render a base on an empty scene
(check-expect (base-draw B0)
              (place-image BASE-IMAGE BASE-X-OFF BASE-Y-OFF MT-SCN))
(check-expect (base-draw B100)
              (place-image BASE-IMAGE (+ 100 BASE-X-OFF) BASE-Y-OFF MT-SCN))
(define (base-draw b)
  (place-image BASE-IMAGE
               (+ (base-x b) (* 1/2 BASE-WIDTH))
               (- SCN-HEIGHT (* 1/2 BASE-HEIGHT))
               MT-SCN))

;; base-key : Base KeyEvent -> Base
;; Move the base left and right in response to left & right arrow keys
(check-expect (base-key B0 "left")
              (make-base (max 0 (- 0 BASE-SPEED)) 3))
(check-expect (base-key BASE1 "left")
              (make-base (max 0 (- 1 BASE-SPEED)) 3))
(check-expect (base-key B0 "right")
              (make-base (min (- SCN-WIDTH BASE-WIDTH) (+ 0 BASE-SPEED)) 3))
(check-expect (base-key BASE5 "right")
              (make-base (min (- SCN-WIDTH BASE-WIDTH) (+ 5 BASE-SPEED)) 3))
(check-expect  (base-key BASEN "right") BASEN)
(check-expect (base-key B0 "a") (make-base 0 3))
(define (base-key b ke)
  (make-base (base-x-key (base-x b) ke)
             (base-lives b)))

(define (base-x-key b ke)
  (cond [(string=? ke "left")
         (max 0 (- b BASE-SPEED))]
        [(string=? ke "right")
         (min BASE-MAX-X (+ b BASE-SPEED))]
        [else b]))

;; base->shot : Base -> Shot
;; Compute the location of a shot fired from given base location
(define (base->shot b)
  (base-x->shot (base-x b)))

;; base-x->shot : Integer -> Shot
;; Compute the location of a shot fired from given base x location
(check-expect (base-x->shot 50) (make-posn (+ 50 BASE-X-OFF) BASE-Y-OFF))
(define (base-x->shot b)
  (make-posn (+ b BASE-X-OFF) BASE-Y-OFF))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timer functions

(define (timer-template t)
  (... (posn-x t) ... (posn-y t) ...))

;; timer-tock : Timer -> Timer
;; Tock the timer one tick, restart countdown if 0
(check-expect (timer-tock (make-posn 0 100))
              (make-posn 99 100))
(check-expect (timer-tock (make-posn 99 100))
              (make-posn 98 100))
(define (timer-tock t)
  (make-posn (sub1 (if (zero? (posn-x t)) (posn-y t) (posn-x t)))
             (posn-y t)))

;; timer-ready? : Timer -> Boolean
;; Is the timer at zero?
(check-expect (timer-ready? (make-posn 0 100)) #true)
(check-expect (timer-ready? (make-posn 100 100)) #false)
(define (timer-ready? t)
  (zero? (posn-x t)))
