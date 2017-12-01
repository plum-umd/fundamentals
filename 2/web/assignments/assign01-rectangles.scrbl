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

@title{Circles}
       
For this exercise, you will develop a structure-based representation
of circles and functions that operate on circles, and then
develop a class-based representation of circles.
        
A @emph{circle} has a radius and color.  They also have a position,
which is given by the coordinates of the center of the circle (using
the graphics coordinates system).

@itemlist[#:style 'ordered 
 @item{@bold{The @racket[circ] structure and functions.}
	                                
        Design a structure-based data representation for
        @tt{Circle} values.  

	Design the functions @racket[=?],
        @racket[area], @racket[move-to], @racket[move-by],
        @racket[stretch], @racket[draw-on], @racket[to-image], @racket[within?],
        @racket[overlap?],
@; @racket[top-left], @racket[top-right],
@;        @racket[bottom-left], @racket[bottom-right], 
and
        @racket[change-color].
        
	@(the-eval 
	   '(begin
(define-struct circ (r c x y))

;; A Circ is a (make-circ Number Color Number Number)

(define (=? c1 c2)
  (and (= (circ-r c1) (circ-r c2))
       (= (circ-x c1) (circ-x c2))
       (= (circ-y c1) (circ-y c2))))

;; area : Circ -> Number
(define (area c)
  (* pi (expt (circ-r c) 2)))

;; move-to : Circ Number Number -> Circ
(define (move-to c x y)
  (circ (circ-r c)
        (circ-c c)
        x
        y))

;; move-by : Circ Number Number -> Circ
(define (move-by c dx dy)
  (circ (circ-r c)
        (circ-c c)
        (+ (circ-x c) dx)
        (+ (circ-y c) dy)))

;; stretch : Circ Number Number -> Circ
(define (stretch r wfactor)
  (circ (* (circ-r r) wfactor)
        (circ-c r)
        (circ-x r)
        (circ-y r)))

;; draw-on : Circ Scene -> Scene
(define (draw-on r scn)
  (place-image (to-image r)
               (circ-x r)
               (circ-y r)
               scn))

;; to-image : Circ -> Image
(define (to-image r)
  (circle (circ-r r) "solid" (circ-c r)))

;; dist : Posn Posn -> Number
(define (dist p1 p2)
  (sqrt (+ (sqr (- (posn-x p1)
                   (posn-x p2)))
           (sqr (- (posn-y p1)
                   (posn-y p2))))))

;; within? : Circ Posn -> Boolean
(define (within? c p)
  (<= (dist (make-posn (circ-x c) (circ-y c)) p)
      (circ-r c)))

@; ;; top-left : Circ -> Posn
@; (define (top-left r)
@;   (make-posn (- (circ-x r) (/ (circ-w r) 2))
@;              (- (circ-y r) (/ (circ-h r) 2))))

@; ;; top-right : Circ -> Posn
@; (define (top-right r)
@;   (make-posn (+ (circ-x r) (/ (circ-w r) 2))
@;              (- (circ-y r) (/ (circ-h r) 2))))

@; ;; bottom-left : Circ -> Posn
@; (define (bottom-left r)
@;  (make-posn (- (circ-x r) (/ (circ-w r) 2))
@;             (+ (circ-y r) (/ (circ-h r) 2))))

@; ;; bottom-right : Circ -> Posn
@; (define (bottom-right r)
@;  (make-posn (+ (circ-x r) (/ (circ-w r) 2))
@;             (+ (circ-y r) (/ (circ-h r) 2))))

;; overlap? : Circ Circ -> Boolean
(define (overlap? r1 r2)
  (or (within? r1 (make-posn (circ-x r2) (circ-y r2)))
      (within? r2 (make-posn (circ-x r1) (circ-y r1)))))

;; change-color : Circ Color -> Circ
(define (change-color r c)
  (circ (circ-r r)
        c
        (circ-x r)
        (circ-y r)))
))

	Here are a few examples to give you some ideas of how the
	functions should work (note you don't necessarily need to use
	the same structure design as used here).
	
	First, let's define a few circles we can use:
	@interaction[#:eval the-eval 
          (define c1 (make-circ 25 "red" 100 70))
          (define c2 (make-circ 50 "blue" 90 30))
          (define c3 (make-circ 10 "green" 50 80))]

	A @racket[(make-circ R C X Y)] is interpreted as a circle
	of radius @racket[R], color @racket[C], and
	centered at position (@racket[X],@racket[Y]) in
	graphics-coordinates.
	
	The @racket[to-image] function turns a circle into an image:
	@interaction[#:eval the-eval
	  (to-image c1)
          (to-image c2)
	  (to-image c3)]
        While the @racket[draw-on] function draws a circle onto a given scene:
        @interaction[#:eval the-eval
 	  (draw-on c1 (empty-scene 200 200))
          (draw-on c2 (empty-scene 200 200))
	  (draw-on c3 (empty-scene 200 200))
	  (draw-on c1 (draw-on c2 (draw-on c3 (empty-scene 200 200))))]
	
        The @racket[area] function computes the area of a circle:
        @interaction[#:eval the-eval
          (area c1)
	  (area c2)
	  (area c3)]

	The @racket[move-to] function moves a circle to be centered
	at the given coordinates:
	@interaction[#:eval the-eval
	  (draw-on (move-to c1 100 100) (empty-scene 200 200))]
 	While @racket[move-by] moves a circle by the given change in coordinates:
	@interaction[#:eval the-eval
	  (draw-on (move-by c1 -30 20) (empty-scene 200 200))]

        The @racket[within?] function tells us whether a given
        position is located within the circle; this includes any
        points on the edge of the circle:
	@interaction[#:eval the-eval
          (within? c1 (make-posn 0 0))
          (within? c1 (make-posn 110 80))]

@;        The @racket[top-left], @racket[top-right],
@;        @racket[bottom-left], and @racket[bottom-right] functions
@;        return coordinates for various corners of a rectangle:
@;	@interaction[#:eval the-eval
@;          (top-left r1)
@;          (top-right r1)
@;          (bottom-left r1)
@;          (bottom-right r1)]

        The @racket[change-color] function produces a circle of the
        given color:
	@interaction[#:eval the-eval
          (to-image (change-color c1 "purple"))]

        The @racket[=?] function compares two circle for equality;
	two rectangles are equal if they have the same radius
	and center point---we ignore color for the purpose of equality:
	@interaction[#:eval the-eval
	  (=? c1 c2)
   	  (=? c1 c1)
	  (=? c1 (change-color c1 "purple"))]
        
	The @racket[stretch] function scales a circle by a given
        factor:
	@interaction[#:eval the-eval
	  (draw-on (stretch c1 3/2) (empty-scene 200 200))]

        The @racket[overlap?] function determines if two circles
        overlap at all:
	@interaction[#:eval the-eval
	  (overlap? c1 c2)
	  (overlap? c2 c1)
 	  (overlap? c1 c3)]

}

 @item{@bold{The @racket[circ%] class.}

        Develop a class-based data representation for @tt{Circle}
        values.  Develop the @emph{methods} corresponding to all the
        functions above.

	The methods should work similar to their functional counterparts:

	@(the-eval 
	   '(begin
(define-class circ% 
  (fields r c x y)
  (define (area)
    (* pi (expt (send this r) 2)))
  (define (draw-on scn)
    (place-image (to-image)
                 (send this x)
                 (send this y)
                 scn))
  (define (to-image)
    (circle (send this r) "solid" (send this c))))
))

	@interaction[#:eval the-eval
	  (define c1 (new circ%  25 "red" 100 70))
	  (define c2 (new circ% 50 "blue" 90 30))
	  (send c1 area)
	  (send c1 draw-on (empty-scene 200 200))
	  ]
}]
