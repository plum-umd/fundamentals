#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner 2htdp/universe) "helper.rkt")
@(require "../utils.rkt")

@lab-title[22]{Focus in a Grid}

@section[#:style 'unnumbered #:tag "lab22:intro"]{Intro}

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

You should start this lab with @link["Lab22.zip"]{this project
skeleton}.


@section[#:style 'unnumbered #:tag "lab22:recall"]{Recall}

In Monday's lab (@labref{21}) we created a @tt{FocusZipper}, a data
structure that allowed us to focus on particular elements inside of a
list. Today, we'll consider what functionality a @tt{FocusZipper} of
@tt{FocusZipper}s enables.

In the file @tt{Zipper.java}, we've given you a fully implemented
@tt{FocusZipper}. We'll use this implementation to build a @tt{Grid},
in which we can freely move up, down, left, or right. This grid will
let us easily implement the game @tt{TicTacToe}! You can run the game
at any time to test the functionality you've implemented so far.


@section[#:style 'unnumbered #:tag "lab22:grid"]{The Grid}

A @tt{FocusZipper<X>} allows us to focus on a particular element of
type @tt{X} inside of a list. When @tt{X} is a @tt{FocusZipper} as
well, we have two different directions in which we can move the
focus. We'll have the inner zippers represent the rows of the grid;
the outer zipper will move between those rows.

To move up and down between rows, we need to move left and right in
the outer zipper.

@bold{Ex 1}: Implement the @tt{Grid.up()} and @tt{Grid.down()}
methods.

To move left and right inside a row, we need to move left and right in
the inner zippers. Note: this is tricker than it seems, make sure that
if you move right, then down, the position is where you expect in the
lower zipper.

@bold{Ex 2}: Implement the @tt{Grid.left()} and @tt{Grid.right()}
methods.

What happens if you use the methods @tt{leftCycle} and @tt{rightCycle}
to implement the grid functionality? Which do you prefer during a game
of @tt{TicTacToe}?


@section[#:style 'unnumbered #:tag "lab22:update"]{Updating the Grid}

A key feature that zippers provide is a way to functionally update a
data structure. By creating a new zipper with a new element in focus,
we've effectively created a new list with a single element changed.

In order to implement TicTacToe, we need to be able to change the
focused square to an @tt{X} or @tt{O}.

@bold{Ex 3}: Design the method @tt{FocusZipper.updateFocus(f)}. It
should take an arbitrary function and given the focused element
@tt{x}, returns a new zipper where the focused element is @tt{f(x)}.



@section[#:style 'unnumbered #:tag "lab22:ttt"]{Winning at Tic Tac Toe}

Now we've got all the pieces we need to play @tt{TicTacToe}, except
it's impossible to win! The method @tt{TicTacToe.gameOver()} should
test to see if either @tt{X} or @tt{O} has won, but it's incomplete.

@bold{Ex 4}: Implement @tt{TicTacToe.gameOver()} inside the file
@tt{Lab22.java}. Hint: use @tt{SquareVisitor}s to do case analysis on
the squares of the grid, and any form of iteration you want to test
all the necessary cases. Feel free to add any operations you need to
the @tt{Listof} class, including @tt{nth}, which may be useful.
