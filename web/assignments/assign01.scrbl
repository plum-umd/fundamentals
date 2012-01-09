#lang scribble/manual
@(require "../utils.rkt"
	  scribble/eval
          racket/sandbox	  
          (for-label class/0))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (only-in lang/htdp-intermediate-lambda local sqr / + sqrt make-posn posn-x posn-y posn?)))
    (the-eval '(require 2htdp/image))
   ;(the-eval '(require lang/htdp-intermediate-lambda))
    (the-eval '(require class/0))
    #;(call-in-sandbox-context 
     the-eval 
     (lambda () ((dynamic-require 'htdp/bsl/runtime 'configure)
                 (dynamic-require 'htdp/isl/lang/reader 'options))))
    the-eval))


@title[#:tag "assign01"]{1/11: Rectangles}

Due: 1/11, midnight. @;[@seclink["soln01"]{Solution}]

Language: @racketmodname[class/0].

       
For this exercise, you will develop a structure-based representation
of rectangles and functions that operate on rectangles, and then
develop a class-based representation of rectangles.
        
A @emph{rectangle} has a width, height, and color.  They also have a
position, which is given by the coordinates of the center of the
rectangle (using the graphics coordinates system).

@itemlist[#:style 'ordered 
 @item{@bold{The @racket[rect] structure and functions.}
	                                
        Design a structure-based data representation for
        @tt{Rectangle} values.  

	Design the functions @racket[=?],
        @racket[area], @racket[move-to], @racket[move-by],
        @racket[stretch], @racket[draw-on], @racket[to-image], @racket[within?],
        @racket[overlap?], @racket[top-left], @racket[top-right],
        @racket[bottom-left], @racket[bottom-right], and
        @racket[change-color].
        
	@(the-eval 
	   '(begin
(define-struct rect (w h c x y))

;; A Rect is a (make-rect Number Number Color Number Number)

(define (=? r1 r2)
  (and (= (rect-w r1) (rect-w r2))
       (= (rect-h r1) (rect-h r2))
       (= (rect-x r1) (rect-x r2))
       (= (rect-y r1) (rect-y r2))))

;; area : Rect -> Number
(define (area r)
  (* (rect-w r) (rect-h r)))

;; move-to : Rect Number Number -> Rect
(define (move-to r x y)
  (rect (rect-w r)
        (rect-h r)
        (rect-c r)
        x
        y))

;; move-by : Rect Number Number -> Rect
(define (move-by r dx dy)
  (rect (rect-w r)
        (rect-h r)
        (rect-c r)
        (+ (rect-x r) dx)
        (+ (rect-y r) dy)))

;; stretch : Rect Number Number -> Rect
(define (stretch r wfactor hfactor)
  (rect (* (rect-w r) wfactor)
        (* (rect-h r) hfactor)
        (rect-c r)
        (rect-x r)
        (rect-y r)))

;; draw-on : Rect Scene -> Scene
(define (draw-on r scn)
  (place-image (to-image r)
               (rect-x r)
               (rect-y r)
               scn))

;; to-image : Rect -> Image
(define (to-image r)
  (rectangle (rect-w r) (rect-h r) "solid" (rect-c r)))              

;; within? : Rect Posn -> Boolean
(define (within? r p)
  (and (<= (- (rect-x r) (/ (rect-w r) 2)) (posn-x p) (+ (rect-x r) (/ (rect-w r) 2)))
       (<= (- (rect-y r) (/ (rect-h r) 2)) (posn-y p) (+ (rect-y r) (/ (rect-h r) 2)))))

;; top-left : Rect -> Posn
(define (top-left r)
  (make-posn (- (rect-x r) (/ (rect-w r) 2))
             (- (rect-y r) (/ (rect-h r) 2))))

;; top-right : Rect -> Posn
(define (top-right r)
  (make-posn (+ (rect-x r) (/ (rect-w r) 2))
             (- (rect-y r) (/ (rect-h r) 2))))

;; bottom-left : Rect -> Posn
(define (bottom-left r)
  (make-posn (- (rect-x r) (/ (rect-w r) 2))
             (+ (rect-y r) (/ (rect-h r) 2))))

;; bottom-right : Rect -> Posn
(define (bottom-right r)
  (make-posn (+ (rect-x r) (/ (rect-w r) 2))
             (- (rect-y r) (/ (rect-h r) 2))))

;; overlap? : Rect Rect -> Boolean
(define (overlap? r1 r2)
  (local [(define (corner-inside? r1 r2)
            (or (within? r1 (top-left r2))
                (within? r1 (top-right r2))
                (within? r1 (bottom-left r2))
                (within? r1 (bottom-right r2))))]
    (or (corner-inside? r1 r2)
        (corner-inside? r2 r1))))
      
;; change-color : Rect Color -> Rect
(define (change-color r c)
  (rect (rect-w r)
        (rect-h r)
        c
        (rect-x r)
        (rect-y r)))
))

	Here are a few examples to give you some ideas of how the
	functions should work (note you don't necessarily need to use
	the same structure design as used here).
	
	First, let's define a few rectangles we can use:
	@interaction[#:eval the-eval 
          (define r1 (make-rect 50 75 "red" 100 70))
          (define r2 (make-rect 100 30 "blue" 90 30))
          (define r3 (make-rect 20 40 "green" 50 80))]

	A @racket[(make-rect W H C X Y)] is interpreted as a rectangle
	of width @racket[W], height @racket[H], color @racket[C], and
	centered at position (@racket[X],@racket[Y]) in
	graphics-coordinates.
	
	The @racket[to-image] function turns a rectangle into an image:
	@interaction[#:eval the-eval
	  (to-image r1)
          (to-image r2)
	  (to-image r3)]
        While the @racket[draw-on] function draws a rectangle onto a given scene:
        @interaction[#:eval the-eval
 	  (draw-on r1 (empty-scene 200 200))
          (draw-on r2 (empty-scene 200 200))
	  (draw-on r3 (empty-scene 200 200))
	  (draw-on r1 (draw-on r2 (draw-on r3 (empty-scene 200 200))))]
	
        The @racket[area] function computes the area of a rectangle:
        @interaction[#:eval the-eval
          (area r1)
	  (area r2)
	  (area r3)]

	The @racket[move-to] function moves a rectangle to be centered
	at the given coordinates:
	@interaction[#:eval the-eval
	  (draw-on (move-to r1 100 100) (empty-scene 200 200))]
 	While @racket[move-by] moves a rectangle by the given change in coordinates:
	@interaction[#:eval the-eval
	  (draw-on (move-by r1 -30 20) (empty-scene 200 200))]

        The @racket[within?] function tells us whether a given
        position is located within the rectangle; this includes any
        points on the edge of the rectangle:
	@interaction[#:eval the-eval
          (within? r1 (make-posn 0 0))
          (within? r1 (make-posn 110 80))]

        The @racket[top-left], @racket[top-right],
        @racket[bottom-left], and @racket[bottom-right] functions
        return coordinates for various corners of a rectangle:
	@interaction[#:eval the-eval
          (top-left r1)
          (top-right r1)
          (bottom-left r1)
 	  (bottom-right r1)]

        The @racket[change-color] function produces a rectangle of the
        given color:
	@interaction[#:eval the-eval
          (to-image (change-color r1 "purple"))]

        The @racket[=?] function compares two rectangles for equality;
	two rectangles are equal if they have the same width, height,
	and center point---we ignore color for the purpose of equality:
	@interaction[#:eval the-eval
	  (=? r1 r2)
   	  (=? r1 r1)
	  (=? r1 (change-color r1 "purple"))]
        
	The @racket[stretch] function scales a rectangle by a given
        horizontal and vertical factor:
	@interaction[#:eval the-eval
	  (draw-on (stretch r1 3/2 1/2) (empty-scene 200 200))]

        The @racket[overlap?] function determines if two rectangles
        overlap at all:
	@interaction[#:eval the-eval
	  (overlap? r1 r2)
	  (overlap? r2 r1)
 	  (overlap? r1 r3)]

}

 @item{@bold{The @racket[rect%] class.}

        Develop a class-based data representation for @tt{Rectangle}
        values.  Develop the @emph{methods} corresponding to all the
        functions above.                

	The methods should work similar to their functional counterparts:

	@(the-eval 
	   '(begin
(define-class rect% 
  (fields w h c x y)
  (define/public (area)
    (* (field w) (field h)))
  (define/public (draw-on scn)
    (place-image (to-image)
                 (field x)
                 (field y)
                 scn))
  (define/public (to-image)
    (rectangle (field w) (field h) "solid" (field c))))
))

	@interaction[#:eval the-eval
	  (define r1 (new rect%  50 75 "red" 100 70))
	  (define r2 (new rect% 100 30 "blue" 90 30))
	  (send r1 area)
	  (send r1 draw-on (empty-scene 200 200))
	  ]


	
}]
