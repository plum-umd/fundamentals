#lang scribble/manual
@(require racket/sandbox
          scribble/example
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ... check-expect))
          (for-label (except-in class/0 define-struct ... check-expect))
          (for-label class/universe)
          "../utils.rkt")

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class/0))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    (the-eval '(require "lectures/6/lon.rkt"))
    the-eval))

@lecture-title[6]{Interface Design: Independent and Extensible}

@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=0c56edcc-41cd-4f14-94f5-a87e01454892"]{Video}.

In this lecture, we explore the interface-based way of defining objects
and observe that it enables two important properties of programs:

@itemlist[
@item{@bold{Representation independence}: programs that use objects
only according to their interface cannot observe, and therefore cannot
depend upon, the representation of those objects.  This enables the
designer of the object to choose the representation in any way they
see fit.}

@item{@bold{Extensibility}: programs that are designed using
interfaces can be extended by adding new kinds of objects that
implement the interface without requiring any changes to existing
code.  This leads to a kind of modularity and separation of concerns that
is important in designing and maintaining large software systems.}]


Let's consider the alternative characterization of lights not in terms
of @emph{what they are}, but rather @emph{what they do} that we say in
@lecref{5}.  A light does two things: it can render as an image
and it can transition to the next light; hence our @emph{interface
definition} for a light is:

@class-block{
;; A Light implements
;; on-tick : -> Light
;; Next light after this light.
;; to-draw : -> Image
;; Draw this light.
}

Now it's clear that each of the three light classes define sets of
objects which are @tt{Light}s, because each implements the methods in
the @tt{Light} interface, but we can imagine new kinds of
implementations of the @tt{Light}.  For example, here's a class that
implements the @tt{Light} interface:

@class-block{
;; A ModLight is a (new mod-light% Natural)
;; Interp: 0 = green, 1 = yellow, otherwise red.
(define-class mod-light%
  (fields n)
  ;; on-tick : -> Light
  ;; Next light after this light.
  (define (on-tick)
    (new mod-light% (modulo (add1 (send this n)) 3)))

  ;; draw : -> Image
  ;; Draw this light.
  (define (to-draw)
    (cond [(= (send this n) 0)
           (circle LIGHT-RADIUS "solid" "green")]
          [(= (send this n) 1)
           (circle LIGHT-RADIUS "solid" "yellow")]
          [else
           (circle LIGHT-RADIUS "solid" "red")])))
}

Notice that every @tt{ModLight} is a @tt{Light}.  Moreover, any
program that is written to use @tt{Light}s will be compatible with any
implemention of the @tt{Light} interface, regardless of its
representation. So notice that the world program only assumes that its
@tt{light} field is a @tt{Light}; this is easy to inspect---the world
never assumes the light is constructed in a particular way, it just
calls @racket[on-tick] and @racket[draw].  Which means that if we were
to start our program off with

@class-block{
(big-bang (new mod-light% 2))
}

it would work exactly as before.

We've now developed a new concept, that of an @emph{interface}, which
is a collection of method signatures.  We say that an object @emph{is}
an instance of an interface whenever it implements the methods of the
interface.

The idea of an interface is already hinted at in the concept of a
union of objects since a function over a union of data is naturally
written as a method in each class variant of the union.  In other
words, to be an element of the union, an object must implement all the
methods defined for the union---the object must implement the union's
interface.  But interfaces are about more than just unions.  By
focusing on interfaces, we can see there are two important engineering
principles that can be distilled even from this small program:

@itemlist[#:style 'ordered
  @item{Representation independence

As we've seen with the simple world program that contains a light,
when a program is written to use only the methods specified in an
interface, then the program is @emph{representation independent} with
respect to the interface; we can swap out any implementation of the
interface without changing the behavior of the program.}

  @item{Extensibility

When we write interface-oriented programs, it's easy to see that they
are @emph{extensible} since we can always design new implementations
of an interface.  Compare this to the construction-oriented view of
programs, which defines a set of values once and for all.}]

These points become increasingly important as we design larger and
larger programs.  Real programs consist of multiple interacting
components, often written by different people.  Representation
independence allows us to exchange and refine components with some
confidence that the whole system will still work after the change.
Extensibility allows us to add functionality to existing programs
without having to change the code that's already been written; that's
good since in a larger project, it may not even be possible to edit a
component written by somebody else.

Let's look at the extensiblity point in more detail.  Imagine we had
developed the @tt{Light} data definition and its functionality along
the lines of @emph{HtDP}.  We would have (we omit @racket[draw] for
now):

@class-block{
;; A Light is one of:
;; - "Red"
;; - "Green"
;; - "Yellow"

;; light-tick : Light -> Light
;; Next light after the given light
(check-expect (light-tick "Green") "Yellow")
(check-expect (light-tick "Red") "Green")
(check-expect (light-tick "Yellow") "Red")
(define (light-tick l)
  (cond [(string=? "Red" l) "Green"]
        [(string=? "Green" l) "Yellow"]
        [(string=? "Yellow" l) "Red"]))
}

Now imagine if we wanted to add a new kind of light---perhaps to
represent a blinking yellow light.  For such lights, let's assume
the next light is just a blinking yellow light:

@class-block{
(check-expect (light-tick "BlinkingYellow") "BlinkingYellow")
}

That's no big deal to implement @emph{if we're allowed to revise
@racket[light-tick]}---we just add another clause to @racket[light-tick] handle
@racket["BlinkingYellow"] lights.  But what if we can't?  What if
@racket[light-tick] were part of a module provided as a library?  Well then
life is more complicated; we'd have to write a new function, say
@racket[fancy-tick], that handled blinking lights and used
@racket[light-tick] for all non-blinking lights.  And while that gets us a
new function with the desired behavior, that won't do anything for all
the places the @racket[light-tick] function is used.  If we're able to edit
the code that uses @racket[light-tick], then we can replace each use of
@racket[light-tick] with @racket[fancy-tick], but what if we can't...?  Well
then we're just stuck.  If we cannot change the definition of
@racket[light-tick] or all the places it is used, then it is not possible to
extend the behavior of @racket[light-tick].

Now let's compare this situation to one in which the original program
was developed with objects and interfaces.  In this situation we have
an interface for lights and several classes, namely @racket[red%],
@racket[yellow%], and @racket[green%] that implement the @racket[on-tick]
method.  Now what's involved if we want to add a variant of lights
that represents a blinking yellow light?  We just need to write a
class that implements @racket[on-tick]:

@class-block{
;; Interp: blinking yellow light
(define-class blinking-yellow%
  ;; on-tick : -> Light
  ;; Next light after this blinking yellow light.
  (check-expect (send (new blinking-yellow%) on-tick)
                (new blinking-yellow%))
  (define (next) this))
}

Notice how we didn't need to edit @racket[red%], @racket[yellow%], or
@racket[green%] at all!  So if those things are set in stone, that's
no problem.  Likewise, programs that were written to use the light
interface will now work even for blinking lights.  We don't need to
edit any uses of the @racket[on-tick] method in order to make it work for
blinking lights.  This program is truly extensible.

@margin-note{@bold{Representation independent testing}: Our current
approach to testing is oriented around the idea of testing for
structurally equal representations of values.  Testing in a
representation independent way requires it's own technique, which we
will see later in the course.}

@section{An Exercise: Developing Lists of Numbers}

A new look at old friend.  Let's look at an object-oriented
development of a list of numbers, in particular we will start with an
interface-based view of these lists, meaning we will consider the
behaviors of a list of numbers (the things we can compute in terms of
lists of numbers) rather than their structure.

Let's start by picking a couple of computations which can be done in
terms of a list of numers: @tt{length} and @tt{map}.

@class-block{
;; A LoN implements:
;; - length : -> Number
;;   Compute the length of this of numbers 
;;   
;; - map : [Number -> Number] -> LoN
;;   Apply the given function to each element of this list of numbers
}

Now we have to think about designing an actual representation of a
list of numnbers.  Here we just follow the same design (albeit with
objects) as we did last semester, using a recursive union data
definition:

@class-block{
;; INTERP: Empty list of numbers
(define-class empty-lon%)

;; INTERP: Non-empty lists of numbers
(define-class cons-lon%
  (fields first rest))
}

Using this representation, we can implement the @racket[LoN] interface
by implementing the @tt{length} and @tt{map} methods. 

Let's start with @tt{empty-lon%}.  First we should declare our intention
that @tt{empty-lon%} objects implement the @tt{LoN} interface:

@class-block{
;; A (new empty-lon%) implements LoN
;; INTERP: Empty list of numbers
(define-class empty-lon%)
}


To fulfill this intention, we must actually define the methods listed
in the @tt{LoN} interface.

Let's make some examples of what these methods should produce in the
case of an empty list of numbers.

@examples[#:eval the-eval
(send (new empty-lon%) length)
(send (new empty-lon%) map add1)]

Writing the code for both is now obvious given the examples:

@filebox[
 (racket empty-lon%)
 @class-block{
 ;; Compute the length of this empty list of numbers
 (check-expect (send (new empty-lon%) length) 0)
 (define (length) 0)

;; map : [Number -> Number] -> LoN
;; Apply the given function to each element of this empty list of numbers
(check-expect (send (new empty-lon%) map add1) (new empty-lon%))
(define (map f) (new empty-lon%))
}]

(Notice how the purpose statements are specialized for the particular
class in which we are defining the methods.)

Moving on to @tt{cons-lon%}, we go through the same steps.  Declare our
intention that @tt{cons-lon%} implements @tt{LoN} and make examples:

@class-block{
;; A (new cons-lon% Number LoN) implements LoN
;; INTERP: Non-empty list of numbers
(define-class cons-lon%
  (fields first rest))
}

@examples[#:eval the-eval
(send (new cons-lon% 3 (new cons-lon% 7 (new empty-lon%))) length)
(send (new cons-lon% 3 (new cons-lon% 7 (new empty-lon%))) map add1)
]

Since the @tt{cons-lon%} class contains some data, it is useful to
consider the template for @tt{cons-lon%} methods:
@filebox[
 (racket empty-lon%)
 @class-block{
 (define (cons-template ...)
   (send this first) ...
   (send (send this rest) cons-template ...))}]


Instantiating the template for @tt{length} and @tt{map} and adjusting the parameters, we get:

@filebox[
 (racket empty-lon%)
 @class-block{
 (define (length)
   (send this first) ...
   (send (send this rest) length))

 (define (map f)
   (send this first) ...
   (send (send this rest) map f))
}]

Combining our knowledge of the examples and the partially filled in
templates, we can see that @racket[(send this first)] stands for
@racket[3] and @racket[(send this rest)] stands for @racket[(new cons-lon%
7 (new empty-lon%))].  Let's make some more examples, based on the list
contained in the @tt{rest} field.

@examples[#:eval the-eval
(send (new cons-lon% 7 (new empty-lon%)) length)
(send (new cons-lon% 7 (new empty-lon%)) map add1)
]

So in the original example, @tt{(send (send this rest) length)} is @racket[1]
and @tt{(send (send this rest) map add1)} is @racket[(new cons-lon% 8 (new empty-lon%))].

Our goal in @tt{length} is @racket[2], which can be computed by
applying @tt{add1} to @racket[(send (send this rest) length)].

Our goal in @tt{map} is @racket[(new cons-lon% 4 (new cons-lon% 8 (new
empty-lon%)))], which can be computed by applying @tt{f} to @racket[(send
this first)] and cons that result onto @racket[(send (send this rest)
map add1)].  We are now in a position to write the code:

@filebox[
 (racket cons-lon%)
 @class-block{
 ;; length : -> Number
 ;; Compute the length of this non-empty list of numbers
 (check-expect (send (new cons-lon% 3 (new cons-lon% 7 (new empty-lon%))) length)
               (new cons-lon% 4 (new cons-lon% 8 (new empty-lon%))))
 (define (length)
   (add1 (send (send this rest) length)))

;; map : [Number -> Number] -> LoN
;; Apply given function to each element of this non-empty list of numbers
(check-expect (send (new cons-lon% 3 (new cons-lon% 7 (new empty-lon%))) map add1)
              (new cons-lon% 4 (new cons-lon% 8 (new empty-lon%))))
(define (map f)
  (new cons-lon% (f (send this first)) (send (send this rest) map f)))
}]


We've now accomplished our initial goal.  On your own, work out a
similar development for lists of strings.
