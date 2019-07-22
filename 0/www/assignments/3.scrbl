#lang scribble/manual
@(require scribble/core)
@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@title[#:style 'unnumbered #:tag "assign3"]{Assignment 3: Design Within Reach}


@bold{Due}: Friday, July 26, 11:59:59 PM EST.


In this assignment, you will design your video game.  You may complete
this assignment individually or in cooperation with a single partner.
You may not work with a group larger than two.

Your design will be evaluated based on its adherence to the design
recipe.  It will not be evaluated for its quality as an entertaining
or enjoyable video game.

It must involve some kind of arbitrarily large collection of data
(i.e. a list).

It must be implemented in ISL or ISL+.

It should be interactive, responding either to mouse or keyboard
events (or both).

It should should be organized with a main function at the top of the
code with a concise description of how to run and play the game.

Below the main function, there should be a section of defined
constants and data definitions.

Function definitions should be organized into sections based on the
kind of data they operate on.

Functions should be thoroughly tested.  As a baseline, every piece of
code (with the exception of the main function) should be evaluated at
least once when evaluating the test cases in the code.

Turn-based multi-player games are fine.  Single-player games are fine.
Simultaneously two-player games that can be played on a single
keyboard are fine.

Multi-player distributed games are fine, but will require you to read
more about how to build such systems in @racketmodname[2htdp/universe].

You may not use any previously assigned game unless it is signficantly
expanded upon.  When in doubt, ask for clarification on the discussion
forum.







