#lang scribble/manual
@(require "unnumbered.rkt")
@(require "utils.rkt"
	  racket/runtime-path
	  scribble/eval
          racket/sandbox)

@(require (for-label (except-in class/0 check-expect)))
@(require (for-label (only-in lang/htdp-intermediate-lambda check-expect)))
@(require (for-label class/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (only-in lang/htdp-intermediate-lambda local sqr / + sqrt make-posn posn-x posn-y posn?)))
    (the-eval '(require 2htdp/image))
   ;(the-eval '(require lang/htdp-intermediate-lambda))
    (the-eval '(require class/2))
    #;(call-in-sandbox-context
     the-eval
     (lambda () ((dynamic-require 'htdp/bsl/runtime 'configure)
                 (dynamic-require 'htdp/isl/lang/reader 'options))))
    the-eval))

@title*{Blog}

@section*{Welcome to CS2510H}

@tt{Fri Jan  4 16:06:59 EST 2013} 

We hope you'll have fun.
