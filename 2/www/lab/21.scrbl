#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner 2htdp/universe) "helper.rkt")
@(require "../utils.rkt")

@lab-title[21]{Focus with Zippers}

@section[#:style 'unnumbered #:tag "lab21:intro"]{Intro}

You'll work in this lab with your
@link["https://piazza.com/class/jcspfhewmdn41y?cid=108"]{lab partners}.

The two of you will work as a team to solve problems. At any time, one
of you will be the @bold{Head} and the other will be the
@bold{Hands}. The @bold{Head} does the thinking and the @bold{Hands}
does the typing. @bold{Hands} type only what the @bold{Head} tells
them to, but you're free to discuss any issues that pop up. You should
switch off during the lab to make sure each of you get practice
problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

You should start this lab with @link["Lab21.zip"]{this project
skeleton}.


@section[#:style 'unnumbered #:tag "lab21:recall"]{Recall}

During last Friday's lecture we implemented a @tt{ListZipper}. This
data structure allowed us to perform fast iteration in any direction
inside a list. We've had experience with zippers last semester, when
we implemented editable textboxes in the student languages.


@section[#:style 'unnumbered #:tag "lab21:focus"]{Zipper with Focus}

Today we're going to implement a slight variation on a
@tt{ListZipper}: a @tt{FocusZipper}.

Consider the textbox: the cursor sits between two lists of
characters. As you enter text the context is extended with additional
characters. You can easily move backward and forward through the text,
and at any point the cursor is @emph{between} two elements in the
zipper.

Now consider a playlist of music: there are songs that have already
played, @emph{one song currently playing}, and songs that have yet to
play. We can play the @tt{previous} song with a single step and we can
play the @tt{next} song with a single step, but there is always one
song currently playing.

While a list zipper places the focus between two elements, a focus
zipper places the focus on a particular element.

@bold{Ex 1}: Implement the constructor of a @tt{FocusZipper}. It must
set the focus on the first element of the given list and throw a
runtime exception if there are no elements in the given list.

@bold{Ex 2}: Design the methods @tt{FocusZipper.left()} and
@tt{FocusZipper.right()}. If there are no more elements to the left or
right (resp.), you should return the current @tt{FocusZipper}.

@bold{Ex 3}: Design the methods @tt{FocusZipper.start()} and
@tt{FocusZipper.end()}, which focus on the first and last elements in
the zipper, resp.


@section[#:style 'unnumbered #:tag "lab21:repeat"]{Adding
functionality: Repeat}

A common bit of functionality in a playlist is a repeat option, which
will restart the playlist from the first song once the last song has
ended.

@bold{Ex 4}: Design the methods @tt{FocusZipper.leftCycle()} and
@tt{FocusZipper.rightCycle()}, which work similarly to the @tt{left}
and @tt{right} you've already implemented. Instead of stopping at the
first and last elements in the zipper (resp.), these methods should
loop to the other side of the zipper.


@section[#:style 'unnumbered #:tag "lab21:random"]{Adding
functionality: Randomize}

Another common bit of functionality in a playlist is a random option,
which will randomly choose songs in the playlist to play. We need a
few more helper methods to implement a random song choice.

@bold{Ex 5}: Design the method @tt{FocusZipper.count()}, which
returns the @tt{Integer} count of all the elements in zipper.

@bold{Ex 6}: Design the method @tt{FocusZipper.nth(Integer n)}, which
shifts focus in the zipper @tt{n} elements to the @tt{right} of the
current zipper. For simplicity, assume that @tt{n} is non-negative and
use @tt{rightCycle} in your implementation.

@bold{Ex 7}: Implement the method @tt{FocusZipper.random()}, which
shifts focus to a random element in the zipper.
