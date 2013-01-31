#lang scribble/manual
@(require "../utils.rkt"
	  "../unnumbered.rkt"
          (for-label (except-in class/1 empty cons first rest list-ref length e check-expect))
          (for-label (only-in lang/htdp-intermediate-lambda check-expect))
	  (for-label class/universe))

@title[#:tag "assign06"]{2/6: Nesting Worlds and Quick Lists}

Due: 2/6.

Language: @racketmodname[class/1]

In this problem set, we'll be implementing operating systems---very
simple ones, but operating systems none-the-less, and a variant of
lists that make @racket[list-ref] a @emph{screamingly} fast operation.
Both problems require you to think carefully about programming to
interfaces.

@section{A boxed @tt{world}}

In this problem, you will implement a wrapper around worlds, which
will display a world with a big border around it.

A @tt{World} implements the following interface:

@codeblock{
;; on-tick : -> World
;; to-draw : -> Image
;; on-key : KeyEvent -> World
;; on-mouse : MouseEvent Number Number -> World
}

Every @tt{World} must produce a 200 by 200 image from the @tt{to-draw}
method.  

Your wrapper around @tt{World}s will be a class that contains a
@tt{World}.  You should then implement a @r[big-bang] program that
displays the wrapped @tt{World} and provides tick, key, and mouse
events to the wrapped @tt{World}. The world should be displayed with a
50 pixel border on all sides.  

Your world should provide tick events to the nested world at the rate
of @racket[(/ 1 28)].  Your world should provide all @tt{KeyEvent}s to
the nested world, @emph{except} the event @racket["q"], which should
end the entire @r[big-bang] program.

Your world should provide mouse events to the nested world, but
@emph{only} when the mouse is located over the 200 by 200 area showing
the nested world.  Your mouse events should be translated to the the
coordinates that the nested world expects.  This means that when the
mouse is over the top left corner of the @emph{nested} world, the
mouse events should have coordinates 0 and 0, not 50 and 50.

You do not need to generate any additional @racket["mouse-enter"] or
@racket["mouse-leave"] events.  This means that the nested worlds will
receive somewhat fewer mouse events they they would if they were not
nested.

@section{A pair of boxed @tt{world}s}

Now, you should extend your implementation to handle two worlds,
side-by-side.  Note that these can be two @emph{different} nested
worlds.  Again, the worlds have a fixed size of 200 by 200.  Mouse
events should be passed to the world the mouse is over, translated so
that the coordinates are appropriate for that world.    Key events
should be passed to both worlds, and both worlds should tick at the
same rate.  

Again, the @racket["q"] key should quit the entire @racket[big-bang]
program.  


Again, you do not need to generate any additional
@racket["mouse-enter"] or @racket["mouse-leave"] events.  This means
that the nested worlds will receive somewhat fewer mouse events they
they would if they were not nested.

@section{Quick Lists}

Van Horn has always been underwhelmed by the fact that
@racket[list-ref] is such a slow operation when you're accessing
elements deep down in a big list.  Why should it take a million
@racket[rest]s just to get the millionth element?

To combat this drawback of an otherwise lovely data structure, the
list, Van Horn has devised an idea for a new implementation of lists
that would let you get the millionth element in about 20 operations.
If his idea works, the @racket[list-ref] operation will take roughly
@emph{log(i)} steps to get the @emph{i}th element.  The other list
operations, on the other hand, would remain more or less just as
efficient as before; taking the rest of a list, for example, might
take a few more steps to compute, but it would be some small constant
number of extra steps.  In the end, we'd have something that behaves
just like a list, but with a much better @racket[list-ref] operation.

Your task is to take Van Horn's idea and implement it.  Since you'll
be building a new kind of list data structure, let's first agree on
the list interface we want:

@codeblock{
;; A [List X] implements
;; - cons : X -> [List X]
;;   Cons given element on to this list.
;; - first : -> X
;;   Get the first element of this list (only defined on non-empty lists).
;; - rest : -> [List X]
;;   Get the rest of this (only defined on non-empty lists).
;; - list-ref : Natural -> X
;;   Get the ith element of this list (only defined for lists of i+1 or more elements).
;; - length : -> Natural
;;   Compute the number of elements in this list.

;; empty is a [List X] for any X.
}

In other words, you have to make an object named @racket[empty] that
implements the list interface above.  Lists should work just like
we're used to, so for example, these tests should all pass if
@racket[empty] is appropriately defined:

@codeblock{
#lang class/1
(require "your-implementation-of-lists.rkt") ; provides empty

(define ls (empty . cons 'a . cons 'b . cons 'c . cons 'd . cons 'e))

(check-expect (empty . length) 0)
(check-expect (ls . length) 5)
(check-expect (ls . first) 'e)
(check-expect (ls . rest . first) 'd)
(check-expect (ls . rest . rest . first) 'c)
(check-expect (ls . rest . rest . rest . first) 'b)
(check-expect (ls . rest . rest . rest . rest . first) 'a)

(check-expect (ls . list-ref 0) 'e)
(check-expect (ls . list-ref 1) 'd)
(check-expect (ls . list-ref 2) 'c)
(check-expect (ls . list-ref 3) 'b)
(check-expect (ls . list-ref 4) 'a)
}

So now let's talk about Van Horn's idea.

Van Horn thinks if instead of representing a list as a ``list of
elements'' you could do better by representing a list as a ``forest
of trees of elements''.  Moreover, the trees will get bigger and
bigger as you go deeper into the forest, and the trees will always be
@emph{full}, meaning if a tree has a left and right subtree, both will
be the same size and full. (For the moment, don't worry about
@emph{why} this makes @racket[list-ref] fast---think about that after
you've implemented Van Horn's idea.)

So here are the key invariants of a @emph{quick list}:

@itemlist[
  @item{A quick list is a forest of increasingly large full binary trees.}
  @item{With the possible exception of the first two trees, every
    successive tree is @emph{strictly} larger.}
]

Now that we have the invariant, let's talk about the operations and
how they both can use and maintain the invariant.

First, @racket[first].  Since the list must be non-empty, we know the
forest has at least one tree, so we can get the first element of the
list by getting ``the first'' element of the tree, which for quick
lists, will be the top element.

Now, @racket[length].  If the forest is empty, the list has length
@racket[0].  If a forest has a tree, the length of the list is the
size of the tree plus the size of the rest of the forest.  (It's
useful to store the size of a tree separately from a tree so that you
don't have to compute it every time you need it.)

The @racket[list-ref] method works as follows: if the index is
@racket[0], the list must be non-empty, so take the first element,
i.e. the top element of the first tree in the forest.  If the index
is non-zero, there are two case: if it's less than the size of the
first tree, the element is in that tree, so fetch it from the first
tree.  If it's larger, adjust the index, and look in the remaining
trees of the forest.

To fetch an element from a tree: if the index is zero, the element is
the top element.  Otherwise, if the index is less than half the size,
it's on the left side; if the index is greater than half, it's on the
right.  (You might do yourself a favor a develop @racket[tree-ref] for
full binary trees and get it working and thoroughly tested before
attempting @racket[list-ref].)

These element-producing operations considered so far have used the
invariant.  Now let's turn to the list-producing operations which must
maintain it.

When an element is @racket[cons]ed, if there at least two trees in the
forest and the first two are the same size, then make a new tree out
of these two and with the given element on top (notice how this tree
is definitely @emph{full}).  Otherwise just make a new tree with one
element and make it the first tree in the forest.

To take the @racket[rest] of a list, there must be at least one tree
in the forest (since the list is non-empty).  We want to split this
tree into its left and right and make these the first two trees in the
forest.  The element that was on top is dropped on the floor and
we're left with a representation of the rest of the list.

And that's that.  When writing your code you want to make sure the
invariants are always true.  Good code should make this fact obvious;
bad code, not so much.

This is a nice little exercise in data structure design and
implementation, and although Van Horn @emph{wishes} this were really
his idea, he actually got it from reading a book by Chris Okasaki, who
has designed a bunch of these kinds of data structures.  Go forth, and
may your @racket[list-ref] never be slow again.