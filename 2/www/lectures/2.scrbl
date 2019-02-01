#lang scribble/manual
@(require "../utils.rkt")

@lecture-title[2]{Unions of Objects}

@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=7f1c24e0-d7e2-4e12-90aa-a9e70106af57"]{Video 2019}.
@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=351c2c35-6a2b-4999-82f1-a8740134139f"]{Video 2018}.

We've already seen the fundamental idea of objects, that an object is
the pairing together of data and functionality.  Over the next few
lectures we will explore what that means in terms of program designs
that we're familiar with.

(In class, we reviewed the basics of how objects and method invocation
work for a simple example of compound data: coordinates.  These topics
are covered in @lecref{1}.)

Let's take a look at a data definition that involves a union.  First,
let's look at the design we are familar with:

@class-block{
;; A Shape is one of:
;; - (make-circ Real)
;; - (make-sq Real)
;; Interp: either a circle or a square

(define-struct circ (radius))
(define-struct sq (size))
}

Here we are defining a set of values called @tt{Shape} and a
@tt{Shape} is either a circle or a square.  Circles are constructed
using the @racket[make-circ] constructor, given a radius for the
circle, and squares are constructed using the @racket[make-sq]
constructor giving the size of the edge of the square.

Knowing the data definition, we can write the template for any
function that operates on shapes.  The template consists of a
@racket[cond] to determine which variant of the union was given as the
argument to the function, and then within each branch of the
@racket[cond], the function can deconstruct the structure by accessing
its fields:

@class-block{
;; shape-template : Shape -> ??
(define (shape-template s)
  (cond [(circ? s) (... (circ-radius s) ...)]
        [(sq? s)   (... (sq-size s) ...)]))
}

Now let's write a particular function on shapes, the function for
computing the area of a shape:

@class-block{
;; area : Shape -> Real
;; Compute the area of the given shape
(check-expect (area (make-sq 3)) 9)
(check-within (area (make-circ 5)) (* 25 pi) 0.0001)
(define (area s)
  (cond [(circ? s) (* pi (sqr (circ-radius s)))]
        [(sq? s) (sqr (sq-size s))]))
}

This is all familiar stuff.  Let's consider what happens when we
switch to using objects.

The first observation to make is that in the above code, we define the
@emph{concept} of @tt{Shape}, but there's no @tt{Shape}
@emph{structure}.  There are only @tt{circ} and @tt{sq} structures.
Analogously, there will not be a @tt{Shape} class, but rather we will
define @tt{Shape} as the union of classes for circles and squares.
Designing the class-based analogs of @tt{circ} and @tt{sq} is pretty
straightforward using the ideas we saw in @lecref{1} for representing
compound data with objects. 

Focusing just on the data definition (not the functionality yet), we get:

@class-block{
;; A Shape is one of:
;; - (new circ% Real)
;; - (new sq% Real)
;; Interp: either a circle or a square

(define-class circ%
  (fields radius))

(define-class sq%
  (fields size))
}

Next we can think about the method for @racket[area].  If we think
about writing a method for each kind of shape, the problem is pretty
simple.  We can write a method computing the area of a circle as
follows:

@filebox[
 (racket circ%)
 @class-block{
 ;; area : -> Real
 ;; Compute the area of this circle
 (define (area)
   (* pi (sqr (send this radius))))
}]

A note on formatting: we use the above convention to mean that the
code belongs inside the @racket[circ%] class definition.  In other
words, we're really saying:

@class-block{
(define-class circ%
  (fields radius)

 ;; area : -> Real
 ;; Compute the area of this circle
 (define (area)
   (* pi (sqr (send this radius)))))
}

The @tt{area} method for squares is straightforward too:

@filebox[
 (racket sq%)
 @class-block{
 ;; area : -> Real
 ;; Compute the area of this square
 (define (area)
   (sqr (send this size)))
}]

Now we have methods for computing the area of squares and the area of
circles, but what about @tt{Shape}s?  Well, we're actually done.
Consider having some value @tt{s} which you know to be a @tt{Shape}.
You can compute its area by sending it the @tt{area} method name,
i.e. @racket[(send s area)].

A natural thing to wonder is @emph{which} method will be used?  The
answer is (just like in the ISL code) ``it depends on what kind of
shape @racket[s] is.''  If @racket[s] was constructed with the
@racket[circ%] constructor, it will use the circle method for
@racket[area]; if it was constructed with @racket[sq%], it will use
the square one.  The key difference, compared with the structure and
function approach is that we, the programmer, don't have to explicitly
write out the case analysis -- the @racket[cond] disappears in the
object-oriented design.

So if the @racket[cond] disappears, how does the object know which
method to use?  The answer here comes back to the fundamental idea of
objects.  Since the object has its functionality coupled together with
its data it doesn't ``figure out'' which method to use, it simply uses
the method that is part of the object.

It will take some getting used to this new style, but this idea lies
at the heart of programming in an object-oriented style.  Instead of
using functions which are independent of data and must explicitly
inspect how the data was constructed in order to figure out what to
compute, we instead couple the appropriate computation with the data
in an object.

Finally, it's worth noting the subtle difference in the signatures and
purpose statements for the object-oriented program (in particular we
go from a single signature and purpose statement for the @racket[area]
function to @emph{two} for the @racket[area] methods).