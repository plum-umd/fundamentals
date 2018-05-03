#lang scribble/manual
@(require scribble/eval
          racket/sandbox
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ... check-expect))
          (for-label (except-in class/0 define-struct ... check-expect))
          (for-label class/universe)
          "../utils.rkt")

@lecture-title[22]{Optional, Maps, Sets, and Lifting Default Code to Abstract Classes}

@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=58fcb844-d7ae-4d0d-9a48-a8b20106566f"]{Video}.