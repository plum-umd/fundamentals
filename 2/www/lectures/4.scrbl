#lang scribble/manual
@(require scribble/eval
          racket/sandbox
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
    (the-eval '(require "lectures/5/light.rkt"))
    the-eval))

@lecture-title[4]{Classes of Objects: Interface Definitions}

@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=388ce64b-fd31-4948-bbaa-a87b01296113"]{Video}.

In this lecture, we take an alternative perspective on defining sets
of objects; we can characterize objects not just by their
construction, as done with a data definition, but also by the methods
they support.  We call this characterization an interface definition.
As we'll see, designing to interfaces leads to generic and extensible
programs.

Let's take another look at the @tt{Light} data definition we developed
in @lecref{4}.  We came up with the following data definition:

@class-block{
;; A Light is one of:
;; - (new red%)
;; - (new green%)
;; - (new yellow%)
}

We started with a @racket[on-tick] method that computes the successor for
each light.  Let's also add a @racket[to-draw] method and then build a
@racket[big-bang] animation for a traffic light.

@codeblock{
#lang class/0
(require 2htdp/image)
(define LIGHT-RADIUS 20)

(define-class red%
  ;; on-tick : -> Light
  ;; Next light after red
  (check-expect (send (new red%) on-tick) (new green%))
  (define (on-tick)
    (new green%))

  ;; to-draw : -> Image
  ;; Draw this red light
  (check-expect (send (new red%) to-draw)
                (circle LIGHT-RADIUS "solid" "red"))
  (define (to-draw)
    (circle LIGHT-RADIUS "solid" "red")))

(define-class green%
  ;; on-tick : -> Light
  ;; Next light after green
  (check-expect (send (new green%) on-tick) (new yellow%))
  (define (on-tick)
    (new yellow%))

  ;; to-draw : -> Image
  ;; Draw this green light
  (check-expect (send (new green%) to-draw)
                (circle LIGHT-RADIUS "solid" "green"))
  (define (to-draw)
    (circle LIGHT-RADIUS "solid" "green")))

(define-class yellow%
  ;; on-tick : -> Light
  ;; Next light after yellow
  (check-expect (send (new yellow%) on-tick) (new red%))
  (define (on-tick)
    (new red%))

  ;; to-draw : -> Image
  ;; Draw this yellow light
  (check-expect (send (new yellow%) to-draw)
                (circle LIGHT-RADIUS "solid" "yellow"))
  (define (to-draw)
    (circle LIGHT-RADIUS "solid" "yellow")))
}

We can now create and view lights:

@interaction[#:eval the-eval
(send (new green%) to-draw)
(send (new yellow%) to-draw)
(send (new red%) to-draw)
]

To create an animation we can make the following @racket[big-bang] program:

@class-block{
(require class/universe)
(big-bang (new red%))
}

At this point, let's take a step back and ask the question: @emph{what
is essential to being a light?}  Our data definition gives us one
perspective, which is that for a value to be a light, that value must
have been constructed with either @racket[(new red%)], @racket[(new
yellow%)], or @racket[(new green%)].  But from the world's
perspective, what matters is not how lights are constructed, but
rather what can lights compute.  All the world does is call methods on
the light it contains, namely the @racket[on-tick] and @racket[to-draw]
methods.  We can rest assured that the light object understands the
@racket[on-tick] and @racket[to-draw] messages because, by definition, a
light must be one of @racket[(new red%)], @racket[(new yellow%)], or
@racket[(new green%)], and each of these classes defines @racket[on-tick]
and @racket[to-draw] methods.  But it's possible we could relax the
definition of what it means to be a light by just saying what methods
an object must implement in order to be considered a light.  We can
thus take a constructor-agnostic view of objects by defining a set of
objects in terms of the methods they understand.  We call a set of
method signatures (i.e., name, contract, and purpose statement) an
@emph{interface}.

Let's consider an alternative characterization of lights not in
terms of @emph{what they are}, but rather @emph{what they do}.
Well a light does two things: it can render as an image and it can 
transition to the next light; hence our @emph{interface definition}
for a light is:

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
implementations of the @tt{Light} interface that are not 
the light classes we've considered so far.

In the next lecture, we'll explore some consquences of designing in
terms of interface definitions intead of data definitions.

