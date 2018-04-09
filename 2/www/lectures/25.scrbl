#lang scribble/manual
@(require scribble/eval
          racket/sandbox
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ... check-expect))
          (for-label (except-in class/0 define-struct ... check-expect))
          (for-label class/universe)
          "../utils.rkt")

@lecture-title[25]{Implementing Visitors; Bank Accounts}

@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=1f66f3df-a944-47c5-abc1-a8b2010656dc"]{Video}.