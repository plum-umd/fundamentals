#lang scribble/manual
@(require scribble/core)
@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@title[#:style 'unnumbered #:tag "assign2"]{Assignment 2: Snek}


@bold{Due}: Friday, July 19, 11:59:59 PM EST.

This exercise will be released July 13.
