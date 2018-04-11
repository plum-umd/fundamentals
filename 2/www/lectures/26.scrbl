#lang scribble/manual
@(require scribble/eval
          racket/sandbox
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ... check-expect))
          (for-label (except-in class/0 define-struct ... check-expect))
          (for-label class/universe)
          "../utils.rkt")

@lecture-title[26]{Imperatives: Implicit Communication via Side-Effects}

@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=628c544f-6608-4942-a4c7-a8ba0143e197"]{Video}.
