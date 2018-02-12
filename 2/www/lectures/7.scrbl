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

@lecture-title[7]{Parametric Interface Definitions and Methods}

@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=48f72fe6-8739-484f-90e0-a8800126c62c"]{Video}.

TBD.