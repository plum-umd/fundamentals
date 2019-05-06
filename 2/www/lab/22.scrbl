#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner 2htdp/universe) "helper.rkt")
@(require "../utils.rkt")

@lab-title[22]{More Random-Access List Operations}

@section[#:style 'unnumbered #:tag "lab22:intro"]{Intro}

You'll work in this lab with ad-hoc partners.

The two of you will work as a team to solve problems. At any time, one
of you will be the @bold{Head} and the other will be the
@bold{Hands}. The @bold{Head} does the thinking and the @bold{Hands}
does the typing. @bold{Hands} type only what the @bold{Head} tells
them to, but you're free to discuss any issues that pop up. You should
switch off during the lab to make sure each of you get practice
problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

You should start this lab with @link["Lec2.zip"]{this project
skeleton}.

@section[#:style 'unnumbered #:tag "lab22:recall"]{Recall}

We've now seen how to build a list that combines the best of
(sequential) lists and random-access arrays: random access lists.

In this lab, you'll implement more list operations for random-access
lists.

@section[#:style 'unnumbered #:tag "lab22:rest"]{Implement @tt{rest} for random-access lists}

We ran out of time to implement @tt{rest}, which optionally produces
the rest of the list.  Remember that the rest of the list must
maintain the random-access list invariants.

Here is the high-level idea of how @tt{rest} works: If the list of
nodes is empty, there is no rest.  If the list of nodes is non-empty,
you want to drop the value of that node, but keep the left and right
subtrees (unless they are leaves!).  If the subtrees are nodes, cons
both the left and right subtrees on to the list of nodes.

Convince yourself this maintains the invariants and test your @tt{rest} method.


@section[#:style 'unnumbered #:tag "lab22:foldr"]{Implement @tt{foldr} for random-access lists}

Add the @tt{foldr} signature to your random-access list interface and implement it.

@section[#:style 'unnumbered #:tag "lab22:map"]{Implement @tt{map} for random-access lists}

Add the @tt{map} signature to your random-access list interface and implement it.

You @emph{could} implement @tt{map} in terms of @tt{foldr}.  Is there
any benefit to not doing it this way?

@section[#:style 'unnumbered #:tag "lab22:more"]{Implement more operations for random-access lists}

Try to add the following, do as many as you can:

@itemlist[
@item{@tt{zip}}
@item{@tt{iterator}}
@item{@tt{rev}}
@item{@tt{app}}
]

@section[#:style 'unnumbered #:tag "lab22:submit"]{Submission}

Submit a zip file of your work at the end of lab.
