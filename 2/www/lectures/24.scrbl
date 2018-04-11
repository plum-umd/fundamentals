#lang scribble/manual
@(require scribble/eval
          racket/sandbox
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ... check-expect))
          (for-label (except-in class/0 define-struct ... check-expect))
          (for-label class/universe)
          "../utils.rkt")

@lecture-title[24]{The Visitor Pattern}

@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=9ea091b1-8cec-45b0-ae3d-a8b201065769"]{Video}.