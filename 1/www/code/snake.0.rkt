;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname snake.0) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; -----------------------------------------------------------------------------
;; Main function

(define (main _)
  (big-bang (make-world (make-snake (make-posn 3 0)
                                    (list (make-posn 2 0)
                                          (make-posn 1 0))
                                    "right")
                        (make-posn 10 10))
    [on-tick tock 1/3]
    [to-draw draw]
    ;[on-key click]
    ;[stop-when game-over?]
    ))


;; -----------------------------------------------------------------------------
;; Data Definitions

;; A World is a (make-world Snake Egg)
(define-struct world (snake egg))

;; A Snake is a (make-snake Segment ListofSegment Dir)
(define-struct snake (head tail dir))

(make-snake (make-posn 0 0) '() "right")

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

(define (segments-template segs)
  (cond [(empty? segs) ...]
        [(cons? segs)
         (... (segment-template (first segs))
              (segments-template (rest segs))
              ...)]))
         
(cons (make-posn 0 0) '())

;; A Segment is a Loc
;; Interp: segment of a snake

(define (segment-template s)
  (... (posn-x s) (posn-y s) ...))

;; A Loc (make-posn Integer Integer)
;; Interp: a location in grid coordinates

;; A Dir is one of:
;; - "left"
;; - "right"
;; - "up"
;; - "down"

(define (dir-template d)
  (cond [(string=? d "left") ...]
        [(string=? d "right") ...]
        [(string=? d "up") ...]
        [(string=? d "down") ...]))
        


;; -----------------------------------------------------------------------------
;; Constants

(define GRID-SIZE 20) ; px
(define HEIGHT 40) ; gs
(define WIDTH 20)  ; gs

(define SEGMENT-IMG (circle (* 3/4 GRID-SIZE) "solid" "red"))
(define EGG-IMG (circle (* 1/2 GRID-SIZE) "solid" "green"))

(define SCENE-4x4 (empty-scene (* GRID-SIZE 4) (* GRID-SIZE 4)))
(define SCENE (empty-scene (* GRID-SIZE WIDTH) (* GRID-SIZE HEIGHT)))


;; -----------------------------------------------------------------------------
;; World functions

;; tock : World -> World

(define (tock w)
  (make-world (snake-advance (world-snake w)) (world-egg w)))

;; draw : World -> Image

(define (draw w)
  (snake-draw-on (world-snake w)
                 (egg-draw-on (world-egg w)
                              SCENE)))

;; click : World KeyEvent -> World

;; game-over? : World -> Boolean


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
              (cons (snake-head s) (segments-drop-last (snake-tail s)))
              (snake-dir s)))


;; snake-hitting-wall? : Snake -> Boolean
;; Is the snake hitting a wall?

;; snake-eating-egg? : Snake Egg -> Boolean
;; Is the given snake eating given egg?

;; snake-hitting-self? : Snake -> Boolean
;; Is the snake hitting itself?
(check-expect (snake-hitting-self? (make-snake (make-posn 0 0) '() "right"))
              #false)
(check-expect (snake-hitting-self? (make-snake (make-posn 0 0)
                                               (list (make-posn 0 0))
                                               "right"))
              #true)
(define (snake-hitting-self? s) #false)



;; -----------------------------------------------------------------------------
;; ListofSegment functions

;; listofsegment-advance : ListofSegment Segment -> ListofSegment
;; Advance the given segments


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


;; -----------------------------------------------------------------------------
;; Segment functions

;; segment-advance : Segment Dir -> Segment
;; Advance the given segment in the given direction
(check-expect (segment-advance (make-posn 0 0) "right") (make-posn 1 0))
(check-expect (segment-advance (make-posn 0 0) "down")  (make-posn 0 1))
(define (segment-advance s d)
  (cond [(string=? d "left")  (make-posn (- (posn-x s) 1) (posn-y s))]
        [(string=? d "right") (make-posn (+ (posn-x s) 1) (posn-y s))]
        [(string=? d "up")    (make-posn (posn-x s) (- (posn-y s) 1))]
        [(string=? d "down")  (make-posn (posn-x s) (+ (posn-y s) 1))]))


;; -----------------------------------------------------------------------------
;; Egg functions


;; -----------------------------------------------------------------------------
;; Draw

;; snake-draw-on : Snake Scene -> Scene

(define (snake-draw-on s scn)
  (segment-draw-on (snake-head s)
                   (segments-draw-on (snake-tail s) scn)))

;; segments-draw-on : ListofSegment Scene -> Scene

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

;; loc->grid : Number -> Number
;; Convert grid to px
(check-expect (loc->grid 4) (+ (* 4 GRID-SIZE) (* 1/2 GRID-SIZE)))
(define (loc->grid i)
  (+ (* i GRID-SIZE) (* 1/2 GRID-SIZE)))























  