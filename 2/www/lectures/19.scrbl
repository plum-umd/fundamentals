#lang scribble/manual
@(require scribble/eval
          racket/sandbox
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ... check-expect))
          (for-label (except-in class/0 define-struct ... check-expect))
          (for-label class/universe)
          "../utils.rkt")

@lecture-title[19]{Properties of Equality: Reflexive, Symmetric, Transitive, and Total}

@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=4a2b80eb-390d-4a79-ad6a-a89e013385fc"]{Video}.
