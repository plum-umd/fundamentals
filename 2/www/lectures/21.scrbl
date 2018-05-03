#lang scribble/manual
@(require scribble/eval
          racket/sandbox
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ... check-expect))
          (for-label (except-in class/0 define-struct ... check-expect))
          (for-label class/universe)
          "../utils.rkt")

@lecture-title[20]{Structural Equality with Double Dispatch; Abstracting and Overridding}

@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=fd1637f8-b011-48da-a151-a8a300ec3181"]{Video}.
