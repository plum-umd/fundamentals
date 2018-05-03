#lang scribble/manual
@(require scribble/eval
          racket/sandbox
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ... check-expect))
          (for-label (except-in class/0 define-struct ... check-expect))
          (for-label class/universe)
          "../utils.rkt")

@lecture-title[29]{BSTs, Maps, The Law of HashCode, and Comparable vs Comparators}

@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=5bf78213-0872-4004-a7a2-a8bd0117c758"]{Video}.