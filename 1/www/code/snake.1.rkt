;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname snake.0) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Notes:
;; - updated: tock
;; - fixed bug: snake-advance
;; - world-eating-egg?
;; - snake-eating-egg?
;; - world-eat&grow
;; - snake-grow
;; - snake-change-dir
;; - snake-hitting-wall?
;; - segment-hitting-wall?
;; - snake-hitting-self?
;; - segments-contains?

;; -----------------------------------------------------------------------------
;; Main function

(define (main _)
  (big-bang (make-world (make-snake (make-posn 3 0)
                                    (list (make-posn 1 0)
                                          (make-posn 0 0))
                                    "right")
                        (make-posn 10 0))
    [on-tick tock 1/8]
    [to-draw draw]
    [on-key click]
    [stop-when game-over?]))


;; -----------------------------------------------------------------------------
;; Data Definitions

;; A World is a (make-world Snake Egg)
(define-struct world (snake egg))
#;
(define (world-template w)
  (... (world-snake w)
       (world-egg w) ...))

;; A Snake is a (make-snake Segment ListofSegment Dir)
(define-struct snake (head tail dir))
#;
(make-snake (make-posn 0 0) '() "right")
#;
(define (snake-template s)
  (...  (segment-template (snake-head s))       ;; Segment
        (listofsegment-template (snake-tail s)) ;; ListofSegment
        (dir-template (snake-dir s))            ;; Dir
        ...))

;; An Eggs is one of:
;; - '()
;; - (cons Egg Eggs)
(cons (make-posn 3 4)
      (cons (make-posn 4 9) '()))

;; An Egg is a Loc
;; Interp: location of the egg
(make-posn 4 9)

;; A ListofSegment is one of:
;; - '()
;; - (cons Segment ListofSegment)

#;
(define (segments-template segs)
  (cond [(empty? segs) ...]
        [(cons? segs)
         (... (segment-template (first segs))
              (segments-template (rest segs))
              ...)]))
         
(cons (make-posn 0 0) '())

;; A Segment is a Loc
;; Interp: segment of a snake

#;
(define (segment-template s)
  (... (posn-x s) (posn-y s) ...))

;; A Loc (make-posn Integer Integer)
;; Interp: a location in grid coordinates

;; A Dir is one of:
;; - "left"
;; - "right"
;; - "up"
;; - "down"

#;
(define (dir-template d)
  (cond [(string=? d "left") ...]
        [(string=? d "right") ...]
        [(string=? d "up") ...]
        [(string=? d "down") ...]))
        


;; -----------------------------------------------------------------------------
;; Constants

(define GRID-SIZE 30) ; px
(define HEIGHT 30) ; gs
(define WIDTH 20)  ; gs

(define SEGMENT-IMG (circle (* 3/4 GRID-SIZE) "solid" "red"))
(define EGG-IMG (circle (* 1/2 GRID-SIZE) "solid" "green"))

(define SCENE-4x4 (empty-scene (* GRID-SIZE 4) (* GRID-SIZE 4)))
(define SCENE (empty-scene (* GRID-SIZE WIDTH) (* GRID-SIZE HEIGHT)))

;; Examples
(define S0 (make-snake (make-posn 0 0) '() "right"))
(define S1 (make-snake (make-posn 1 0) '() "right"))
(define S2 (make-snake (make-posn 1 0) (list (make-posn 0 0)) "right"))
(define S3 (make-snake (make-posn -1 0) '() "right"))

(define E0 (make-posn 10 10))
(define E1 (make-posn 0 0))

(define W0 (make-world S0 E0))
(define W1 (make-world S0 E1))
(define W2 (make-world S3 E0))


;; -----------------------------------------------------------------------------
;; World functions

;; tock : World -> World
;; Advance the game one tick of the clock
(check-expect (tock W0) (make-world (snake-advance S0) E0))
(check-random (tock W1) (world-eat&grow W1))
(define (tock w)
  (cond [(world-eating-egg? w) (world-eat&grow w)]
        [else
         (make-world (snake-advance (world-snake w))
                     (world-egg w))]))

;; click : World KeyEvent -> World
;; Change direction of snake if given an arrow key
(check-expect (click W0 "a") W0)
(check-expect (click W0 "left")
              (make-world (snake-change-dir S0 "left") E0))
(define (click w ke)
  (cond [(or (string=? "left" ke)
             (string=? "down" ke)
             (string=? "up" ke)
             (string=? "right" ke))
         (make-world (snake-change-dir (world-snake w) ke)
                     (world-egg w))]
        [else w]))

;; game-over? : World -> Boolean
;; Is the snake dead?
(check-expect (game-over? W0) #false)
(check-expect (game-over? W2) #true)
(define (game-over? w)
  (snake-dead? (world-snake w)))

;; world-eating-egg? : World -> Boolean
;; Is the snake eating an egg in the given world?
(check-expect (world-eating-egg? W0) #false)
(check-expect (world-eating-egg? W1) #true)
(define (world-eating-egg? w)
  (snake-eating-egg? (world-snake w) (world-egg w)))

;; world-eat&grow : World -> World
;; Eat the egg and grow the snake in the given world
(check-random (world-eat&grow W1)
              (make-world (snake-grow S0)
                          (new-egg E1)))
(define (world-eat&grow w)
  (make-world (snake-grow (world-snake w))
              (new-egg (world-egg w))))


;; -----------------------------------------------------------------------------
;; Snake functions

;; snake-advance : Snake -> Snake
;; advance the snake in its direction
(check-expect (snake-advance (make-snake (make-posn 0 0) '() "right"))
              (make-snake (make-posn 1 0) '() "right"))

(check-expect (snake-advance (make-snake (make-posn 1 0)
                                         (list (make-posn 0 0))
                                         "right"))
              (make-snake (make-posn 2 0) (list (make-posn 1 0)) "right"))
(check-expect (snake-advance (make-snake (make-posn 1 0)
                                         (list (make-posn 0 0) (make-posn 0 1))
                                         "right"))
              (make-snake (make-posn 2 0)
                          (list (make-posn 1 0) (make-posn 0 0))
                          "right"))
(define (snake-advance s)
  (make-snake (segment-advance (snake-head s) (snake-dir s))
              (segments-drop-last (cons (snake-head s) (snake-tail s)))
              (snake-dir s)))

;; snake-grow : Snake -> Snake
;; Grow the snake 1 segment in current direction
(check-expect (snake-grow (make-snake (make-posn 0 0) '() "right"))
              (make-snake (make-posn 1 0) (list (make-posn 0 0)) "right"))
(define (snake-grow s)
  (make-snake (segment-advance (snake-head s) (snake-dir s))
              (cons (snake-head s) (snake-tail s))
              (snake-dir s)))

;; snake-change-dir : Snake Dir -> Snake
;; Change the direction of given snake
(check-expect (snake-change-dir (make-snake (make-posn 0 0) '() "right") "left")
              (make-snake (make-posn 0 0) '() "left"))
(define (snake-change-dir s d)
  (make-snake (snake-head s)
              (snake-tail s)
              d))

;; snake-dead? : Snake -> Boolean
(check-expect (snake-dead? (make-snake (make-posn 0 0) '() "right")) #false)
(check-expect (snake-dead? (make-snake (make-posn -1 0) '() "right")) #true)
(check-expect (snake-dead? (make-snake (make-posn 0 0)
                                       (list (make-posn 0 0))
                                       "right"))
              #true)
(define (snake-dead? s)
  (or (snake-hitting-wall? s)
      (snake-hitting-self? s)))

;; snake-hitting-wall? : Snake -> Boolean
;; Is the snake hitting a wall?
(check-expect (snake-hitting-wall? (make-snake (make-posn 0 0) '() "right"))
              #false)
(check-expect (snake-hitting-wall? (make-snake (make-posn -1 0) '() "right"))
              #true)
(define (snake-hitting-wall? s)
  (segment-hitting-wall? (snake-head s)))

;; snake-eating-egg? : Snake Egg -> Boolean
;; Is the given snake eating given egg?
(check-expect (snake-eating-egg? (make-snake (make-posn 0 0) '() "right")
                                 (make-posn 0 0))
              #true)
(check-expect (snake-eating-egg? (make-snake (make-posn 0 0) '() "right")
                                 (make-posn 1 1))
              #false)
(define (snake-eating-egg? s e)
  (loc=? (snake-head s) e))

;; snake-hitting-self? : Snake -> Boolean
;; Is the snake hitting itself?
(check-expect (snake-hitting-self? (make-snake (make-posn 0 0) '() "right"))
              #false)
(check-expect (snake-hitting-self? (make-snake (make-posn 0 0)
                                               (list (make-posn 0 0))
                                               "right"))
              #true)
(define (snake-hitting-self? s)
  (segments-contains? (snake-tail s) (snake-head s)))


;; -----------------------------------------------------------------------------
;; ListofSegment functions

;; segments-drop-last : ListofSegment -> ListofSegment
;; Take all but the last segment of the snake
(check-expect (segments-drop-last '()) '())
(check-expect (segments-drop-last (cons (make-posn 0 0) '())) '())
(check-expect (segments-drop-last (list (make-posn 3 3)
                                        (make-posn 2 3)
                                        (make-posn 1 2)))
              (list (make-posn 3 3)
                    (make-posn 2 3)))                                   
(define (segments-drop-last segs)
  (cond [(empty? segs) '()]
        [(cons? segs)
         (cond [(empty? (rest segs)) '()]
               [else
                (cons (first segs)
                      (segments-drop-last (rest segs)))])]))

;; segments-contains? : ListofSegment Segment -> Boolean
;; Do given segments contain seg?
(check-expect (segments-contains? '() (make-posn 0 0)) #false)
(check-expect (segments-contains? (list (make-posn 0 0)) (make-posn 0 0)) #true)
(check-expect (segments-contains? (list (make-posn 0 0)
                                        (make-posn 1 1))
                                  (make-posn 1 1)) #true)
(define (segments-contains? segs seg)
  (cond [(empty? segs) #false]
        [(cons? segs)
         (or (loc=? (first segs) seg)
             (segments-contains? (rest segs) seg))]))


;; -----------------------------------------------------------------------------
;; Segment functions

;; segment-advance : Segment Dir -> Segment
;; Advance the given segment in the given direction
(check-expect (segment-advance (make-posn 0 0) "left")  (make-posn -1  0))
(check-expect (segment-advance (make-posn 0 0) "right") (make-posn  1  0))
(check-expect (segment-advance (make-posn 0 0) "up")    (make-posn  0 -1))
(check-expect (segment-advance (make-posn 0 0) "down")  (make-posn  0  1))
(define (segment-advance s d)
  (cond [(string=? d "left")  (make-posn (- (posn-x s) 1) (posn-y s))]
        [(string=? d "right") (make-posn (+ (posn-x s) 1) (posn-y s))]
        [(string=? d "up")    (make-posn (posn-x s) (- (posn-y s) 1))]
        [(string=? d "down")  (make-posn (posn-x s) (+ (posn-y s) 1))]))

;; segment-hitting-wall? : Segment -> Boolean
;; Has given segment hit wall (moved past wall)?
(define (segment-hitting-wall? seg)
  (or (= -1 (posn-x seg))
      (= -1 (posn-y seg))
      (= WIDTH (posn-x seg))
      (= HEIGHT (posn-y seg))))


;; -----------------------------------------------------------------------------
;; Egg functions

;; new-egg : Egg -> Egg
;; Construct a new egg at a random location
(check-random (new-egg (make-posn 3 4))
              (make-posn (random WIDTH) (random HEIGHT)))
(define (new-egg e)
  (make-posn (random WIDTH) (random HEIGHT)))


;; -----------------------------------------------------------------------------
;; Loc functions

;; loc->grid : Number -> Number
;; Convert grid to px
(check-expect (loc->grid 4) (+ (* 4 GRID-SIZE) (* 1/2 GRID-SIZE)))
(define (loc->grid i)
  (+ (* i GRID-SIZE) (* 1/2 GRID-SIZE)))

;; loc=? : Loc Loc -> Boolean
;; Are the two locations the same?
(check-expect (loc=? (make-posn 0 0) (make-posn 0 0)) #true)
(check-expect (loc=? (make-posn 0 0) (make-posn 0 1)) #false)
(define (loc=? l1 l2)
  (and (= (posn-x l1) (posn-x l2))
       (= (posn-y l1) (posn-y l2))))


;; -----------------------------------------------------------------------------
;; Draw

;; draw : World -> Image
;; Render the state of the game as an image
(check-expect (draw W0)
              (snake-draw-on S0 (egg-draw-on E0 SCENE)))
(define (draw w)
  (snake-draw-on (world-snake w)
                 (egg-draw-on (world-egg w)
                              SCENE)))

;; snake-draw-on : Snake Scene -> Scene
;; Draw given snake on scene
(check-expect (snake-draw-on S2 SCENE)
              (segment-draw-on (make-posn 1 0)
                               (segment-draw-on (make-posn 0 0)
                                                SCENE)))
(define (snake-draw-on s scn)
  (segment-draw-on (snake-head s)
                   (segments-draw-on (snake-tail s) scn)))

;; segments-draw-on : ListofSegment Scene -> Scene
;; Draw given segments on scene
(check-expect (segments-draw-on '() SCENE) SCENE)
(check-expect (segments-draw-on (list (make-posn 0 0)) SCENE)
              (segment-draw-on (make-posn 0 0) SCENE))
(define (segments-draw-on segs scn)
  (cond [(empty? segs) scn]
        [(cons? segs)
         (segment-draw-on (first segs)
                          (segments-draw-on (rest segs) scn))]))  

;; segment-draw-on : Segment Scene -> Scene
;; Draw the given segment on the scene
(check-expect (segment-draw-on (make-posn 0 0) SCENE-4x4)
              (loc-draw-img-on (make-posn 0 0) SEGMENT-IMG SCENE-4x4))
(define (segment-draw-on s scn)
  (loc-draw-img-on s SEGMENT-IMG scn))

;; segment-draw-on : Egg Scene -> Scene
;; Draw the given egg on the scene
(check-expect (egg-draw-on (make-posn 0 0) SCENE-4x4)
              (loc-draw-img-on (make-posn 0 0) EGG-IMG SCENE-4x4))
(define (egg-draw-on s scn)
  (loc-draw-img-on s EGG-IMG scn))

;; loc-draw-on : Loc Image Scene -> Scene
;; Draw given image at location on scene
(check-expect (loc-draw-img-on (make-posn 3 4) SEGMENT-IMG SCENE-4x4)
              (place-image SEGMENT-IMG (loc->grid 3) (loc->grid 4) SCENE-4x4))
(define (loc-draw-img-on l img scn)
  (place-image img
               (loc->grid (posn-x l))
               (loc->grid (posn-y l))
               scn))


;; -----------------------------------------------------------------------------

;; Pick an element from a non-empty list of elements
(check-random (pick-random (list 1 2 3))
              (list-ref (list 1 2 3) (random 3)))
(define (pick-random xs)
  (list-ref xs (random (length xs))))

  