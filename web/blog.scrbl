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

@section*{Fixing image errors in DrRacket}

@tt{Mon Jan 14 12:21:14 EST 2013}

Sometimes, when you have a file in DrRacket that uses images,
sometimes you'll get the following error

@verbatim{
write: cannot marshal value that is embedded in compiled code
  value: (object:image-snip% ...)
}

To fix this, you should change your DrRacket settings as follows.
First, open the @emph{Choose Language} item from the @emph{Language}
menu.  Then click on @emph{Show Details} Then @bold{uncheck} the
@emph{Populate "compiled" directories} item.

In general, tools such as Git will work better with files that embed
images when you follow the guidelines outlined on the @secref["style"]
page.

@section*{Assignment 2 out, Git notes updated}

@tt{Thu Jan 10 11:29:45 EST 2013}

@seclink["assign02"]{Assignment 2} is posted.  Be forewarned, it is
quite involved so we suggest you start early.

We've also updated the notes on @secref{Git} and flushed out
descriptions of some of the most important Git concepts.

@section*{Name and User name survey}

@tt{Mon Jan  7 16:18:18 EST 2013}

Register your CCIS and GitHub user names with @link["https://docs.google.com/spreadsheet/viewform?formkey=dG1TZk9BYzN0UU54NVhXVzhPR3J1YXc6MQ"]{this survey}.

@section*{Initial Partnerships}

@tt{Mon Jan  7 16:16:06 EST 2013}

Below are the initial partnership assignments.  If you can't find your partner,
or are not on this list, please see a member of the course staff.

@verbatim{
01. Nir Shtuhl               Michael R. Rinaldi
02. Trevyn J. Langsford      Tevin M. Otieno
03. Cody M. Wetherby         Dylan P. Collins
04. Matthew T. Cotton        Maxwell T. Skala
05. Eli L. Abidor            Timothy J. Wright
06. Nicholas L. Jones        Tiffany L. Chao
07. Christopher M. Freeley   Calvin J. Pomerantz
08. Zachary Youngren         Craig J. Ellis
09. Lukas Berger             Julia T. Ebert
10. Samantha T. Sanborn      Lochlainn O. Macdonald
11. Ariel R. Winton          Kevin G. O'Leary
12. Stefan T. Cepko          Sree Vishant Prabhakaran
13. Kyle H. Alpert           James C. Larisch
14. Yang Yang                Christian J. DiMare
15. Kaila M. Corrington      Victor M. Monterroso
16. Joseph S. Maxwell        Victor Lei
17. Zoe R. Winkworth         Priscilla A. Baquerizo
18. Kyle P. Meyer
19. Trithep Devakul          Matthew C. Singer
20. Nicholas D. Acquadro     William Caruso
}

@section*{Welcome to CS2510H}

@tt{Fri Jan  4 16:06:59 EST 2013} 

We hope you'll have fun.
