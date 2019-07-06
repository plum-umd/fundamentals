#lang scribble/manual
@(require scribble/core)
@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@title[#:style 'unnumbered #:tag "assign3"]{Assignment 3: Sneks!}


@bold{Due}: Friday, July 26, 11:59:59 PM EST.

This exercise will be released July 20.

