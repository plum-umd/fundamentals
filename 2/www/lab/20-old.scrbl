#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner 2htdp/universe) "helper.rkt")
@(require "../utils.rkt")

@lab-title[20]{Stacks of Hanoi}

@section[#:style 'unnumbered #:tag "lab20:intro"]{Intro}

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

You should start this lab with @link["Lab20.zip"]{this project
skeleton}.


@section[#:style 'unnumbered #:tag "lab20:tower"]{Tower of Hanoi}

The @link["https://en.wikipedia.org/wiki/Tower_of_Hanoi"]{Tower of
Hanoi} is
@link["https://upload.wikimedia.org/wikipedia/commons/6/60/Tower_of_Hanoi_4.gif"]{simple
puzzle}. There are three posts: the leftmost starts with a stack of
rings ordered in ascending width. The goal is to move the rings from
the leftmost to another post.

The rules of the game:

@itemlist[

  @item{Only one ring can be moved at a time.}

  @item{Only the topmost ring of any tower can be moved.}

  @item{Each ring can only be placed on a larger ring.}

]

We're going to implement this game using ordered stacks in an
imperative world. The partially finished @tt{Hanoi implementing World}
is given in @tt{Lab20.java}.

The classes found in @tt{Listof.java} and @tt{Stackof.java} are mostly
unchanged from @labref{19}. The given classes @tt{Ring} and @tt{Tower}
are simple and need not be changed. Look them over to understand their
purpose.

@bold{Ex 1}: Implement the method @tt{Hanoi.moveRing}. Remember, you
can only make the move if the tower @tt{from} has a ring and the
topmost ring on the tower @tt{to} has either no ring or a larger ring.
@bold{Hint}: A good design starts by creating the method @tt{Boolean
OrdStackof.canPush(X x)}, which returns true if the element @tt{x} can
be pushed onto @tt{this} ordered stack (leaving the stack unchanged).

@bold{Ex 2}: The definition of @tt{canPush} looks a lot like @tt{void
OrdStackof.push(X x)}; reimplement @tt{push} to use @tt{canPush} if
you haven't already.

@bold{Ex 3}: Implement the game logic in @tt{Hanoi.onMouseClicked}. In
our game, we first select some source tower (with a ring on it) by
clicking it, then click on the tower to which we will move the topmost
ring of the source. The translation between the mouse event and the
tower that was clicked is already given. You only need to implement
the game logic to select source and destination towers.

@bold{Ex 4}: The current @tt{Hanoi} class has a hardcoded number of
rings: 3. Generalize the class to work for an arbitrary number of
rings.

@bold{Ex 5}: The current @tt{Hanoi} class has a hardcoded number of
towers: 3. Generalize the class to work for an arbitrary number of
towers.
