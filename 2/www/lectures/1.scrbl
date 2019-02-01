#lang scribble/manual
@(require "../utils.rkt")

@lecture-title[1]{The Essence of Objects}

@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=f534f504-6f4a-4eae-9818-a9e500fbec05"]{Video 2019}.
@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=50a9a419-9c7f-4d0e-ae65-a87400f8b95a"]{Video 2018}.

One of the key concepts behind so-called @deftech{object-oriented
programming} (OOP) is the notion of an @deftech{object}.  An object is
a new kind of value that can, as a first cut, be understood as a
pairing together of two familiar concepts: @as-index{data} and
@as-index{function}.

@itemlist[

@item{@bold{An object is like a structure} in that it has a fixed
number of fields, thus an object (again, like a structure) can
represent @as-index{compound data}.  But unlike a
@as-index{structure}, an object contains not just data, but
@emph{functionality} too;}

@item{@bold{An object is like a (set of) function(s)} in
that it has behavior---it @emph{computes}; it is not just inert
data.}]

This suggests that objects are a natural fit for well-designed
programs since good programs are organized around data definitions and
@as-index{functions} that operate over such data.  An object, in
essence, packages these two things together into a single programming
apparatus.  This has two important consequences:

@itemlist[#:style 'ordered

@item{@bold{You already know how to design programs oriented around
objects.}

Since objects are just the combination of two familiar concepts that
you already use to design programs, you already know how to design
programs around objects, even if you never heard the term ``object''
before.  In short, the better you are at programming with functions,
the better you will be at programming with objects.}

@item{@bold{Objects enable new kinds of @as-index{abstraction} and
@as-index{composition}.}

Although the combination of data and function may seem simple, objects
enable new forms of abstraction and composition.  That is, objects
open up new approaches to the construction of computations.  By
studying these new approaches, we can distill new design principles.
Because we understand objects are just the combination of data and
function, we can understand how all of these principles apply in the
familiar context of programming with functions.  In short, the better
you are at programming with objects, the better you will be at
programming with functions.}]

Let's start by looking at a very simple program developed using the
systematic approach of last semster.  This program operates on
Cartesian coordinates and consists of a couple simple functions for
computing the distance of a coordinate from the orgin and for moving a
point by given offsets:

@#reader scribble/comment-reader
(racketblock
;; A Coord is a (make-coord Real Real)
;; Interp: Cartesian coordinate
(define-struct coord (x y))

;; dist0 : Coord -> Real
;; Distance of given coordinate to origin
(check-expect (dist0 (make-coord 3 4)) 5)
(define (dist0 c)
  (sqrt (+ (sqr (coord-x c))
           (sqr (coord-y c)))))

;; move : Coord Real Real -> Coord
;; Move given coordinate by offsets
(check-expect (move (make-coord 3 4) 2 -1) (make-coord 5 3))
(define (move c dx dy)
  (make-coord (+ (coord-x c) dx)
              (+ (coord-y c) dy)))
)

This program consists of two parts: a data definition for coordinates
and a set of function definitions that operate on coordinates.

An object, is in essence, just the fusing together of those two parts.
Here is a similar program developed using objects:

@#reader scribble/comment-reader
(racketblock
;; A Coord is a (new coord% Real Real)
;; Interp: Cartesian coordinate

(define-class coord%
  (fields x y)

  ;; dist0 : -> Real
  ;; Distance of this coordinate to origin
  (check-expect (send (new coord% 3 4) dist0) 5)
  (define (dist0)
    (sqrt (+ (sqr (send this x))
             (sqr (send this y)))))

  ;; move : Real Real -> Coord
  ;; Move this coordinate by offsets
  (check-expect (send (new coord% 3 4) move 2 -1) (new coord% 5 3))
  (define (move dx dy)
    (new coord%
         (+ (send this x) dx)
         (+ (send this y) dy))))
)

Notice that this programs consists of a single @emph{class}
definition.  A class definition is a mechanism for defining a set of
objects; similar to how a struct definition is a mechanism for
defining a set of structures.  

The @racket[coord%] class defines a new kind of value, which are
objects consisting of two fields @racket[x] and @racket[y], and two
methods @racket[dist0] and @racket[move].

To create an object that is an member of the @racket[coord%] class,
you use @racket[new], the name of the class (@racket[coord%]), and the
appropriate number (and kind) of values to place in the fields of the
object.  For example, @racket[(new coord% 3 4)] will construct a
@tt{Coord} that represents a point at (3,4).  This is analogous to
using the @racket[make-coord] constructor in the original program.

To access the data within an object, you use @racket[send], a
@tt{Coord} value, and the name of the field you'd like to extract.
For example, @racket[(send (new coord% 3 4) x)] will produce
@racket[3], which is the value of the @racket[x] field within the
object @racket[(new coord% 3 4)].  This is analogous to using the
@racket[coord-x] accessor function in the oroginal program.

To compute with an object, you use @racket[send], a @tt{Coord} value,
the name of the method you'd like to compute, and any additional
arguments the method expects.  For example, @racket[(send (new coord%
3 4) dist0)] computes 5, the distance of (3,4) to the origin.  Notice
that this method takes no additional arguments; it is a function of
just the data within the coordinate.  On the other hand, @racket[(send
(new coord% 3 4) move 1 2)] takes two arguments: the change in
@math{x} and @math{y}.  It computes a new coordinate representing
(4,6), namely @racket[(new coord% 4 6)].

Notice that the definitions of @racket[dist0] and @racket[move] make
use of the variable @racket[this], which doesn't seem to be defined
anywhere.  The name @racket[this] is implicitly defined to me ``the
object on which the method was invoked.'' So for example, when
computing @racket[(send (new coord% 3 4) dist0)], the name
@racket[this] means @racket[(new coord% 3 4)] in the body of
@racket[dist0].

Notice that there are subtle changes to the signature and purpose
statement.  Spend some time comparing them and try to formulate
@emph{why} these differences exist.