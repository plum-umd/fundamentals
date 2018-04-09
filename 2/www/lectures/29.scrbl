#lang scribble/manual
@(require scribble/eval
          racket/sandbox
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ... check-expect))
          (for-label (except-in class/0 define-struct ... check-expect))
          (for-label class/universe)
          "../utils.rkt")

@lecture-title[29]{Imperatives: Methods over Cylic Data}

@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=5d1cb66e-9758-44d5-8b2d-a8bd00fc98b3"]{Video}.