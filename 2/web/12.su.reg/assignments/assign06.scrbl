#lang scribble/manual
@(require "../utils.rkt")

@title[#:tag "assign06"]{6/19: Quick Lists}

Due: 6/19, midnight by svn.

Language: Java.

You must complete this assignment with your partner and should not
discuss solutions with anyone but your partner and the course staff.

This assignment is due Tuesday at midnight.

@section[#:tag "assign06lab"]{Follow-up from Lab}

Complete exercises 1-11 from @seclink["lab11"]{Lab 10}.

@section{Quick Lists}

I have always been underwhelmed by the fact that the cons lists that
are so useful only allow a really slow implementation of
@racket[list-ref], aka @tt{get} in Java parlance.  Why should it take
a million @racket[rest]s just to get the millionth element?

To combat this drawback of an otherwise lovely data structure, the
list, CS researchers have devised an idea for a new implementation of
lists that would let you get the millionth element in about 20
operations.  If his idea works, the @tt{get} operation will take
roughly @emph{log(i)} steps to get the @emph{i}th element.  The other
list operations, on the other hand, would remain more or less just as
efficient as before; taking the rest of a list, for example, might
take a few more steps to compute, but it would be some small constant
number of extra steps.  In the end, we'd have something that behaves
just like a list, but with a much better @racket[get] operation.

Your task is to take this idea and implement it.  Since you'll be
building a new kind of list data structure, let's first agree on the
list interface we want.  We'll follow Hoare's example and dub these
lists ``quick lists'':

@verbatim|{
interface QList<X> {
   // Cons given element on to this list.
   QList<X> cons(X x);

   // Get the first element of this list (only defined on non-empty lists).
   X first();

   // Get the rest of this list (only defined on non-empty lists).
   QList<X> rest();

   // Get the ith element of this list
   // (only defined for lists of i+1 or more elements).
   X get(Integer i);

   // Compute the number of elements in this list.
   Integer size();
}

// new QEmpty<X>() should construct an empty quick list of Xs.
}|

So now let's talk about the idea behind this data structure.

Instead of representing a list as a ``list of elements'' you could do
better by representing a list as a ``forest of trees of elements''.
Moreover, the trees will get bigger and bigger as you go deeper into
the forest, and the trees will always be @emph{full}, meaning if a
tree has a left and right subtree, both will be the same size and
full. (For the moment, don't worry about @emph{why} this makes
@tt{get} fast---think about that after you've implemented the idea.)

So here are the key invariants of a @emph{quick list}:

@itemlist[
  @item{A quick list is a forest of increasingly large full binary trees.}
  @item{With the possible exception of the first two trees, every
    successive tree is @emph{strictly} larger.}
]

Now that we have the invariant, let's talk about the operations and
how they both can use and maintain the invariant.

First, @tt{first}.  Since the list must be non-empty, we know the
forest has at least one tree, so we can get the first element of the
list by getting ``the first'' element of the tree, which for quick
lists, will be the top element.

Now, @tt{length}.  If the forest is empty, the list has length
@tt{0}.  If a forest has a tree, the length of the list is the
size of the tree plus the size of the rest of the forest.  (It's
useful to store the size of a tree separately from a tree so that you
don't have to compute it every time you need it.)

The @tt{get} method works as follows: if the index is @tt{0}, the list
must be non-empty, so take the first element, i.e. the top element of
the first tree in the forest.  If the index is non-zero, there are two
case: if it's less than the size of the first tree, the element is in
that tree, so fetch it from the first tree.  If it's larger, adjust
the index, and look in the remaining trees of the forest.

To fetch an element from a tree: if the index is zero, the element is
the top element.  Otherwise, if the index is less than half the size,
it's on the left side; if the index is greater than half, it's on the
right.  (You might do yourself a favor a develop @tt{get} for full
binary trees and get it working and thoroughly tested before
attempting @tt{get} for quick lists.)

These element-producing operations considered so far have used the
invariant.  Now let's turn to the list-producing operations which must
maintain it.

When an element is @tt{cons}ed, if there at least two trees in the
forest and the first two are the same size, then make a new tree out
of these two and with the given element on top (notice how this tree
is definitely @emph{full}).  Otherwise just make a new tree with one
element and make it the first tree in the forest.

To take the @tt{rest} of a list, there must be at least one tree
in the forest (since the list is non-empty).  We want to split this
tree into its left and right and make these the first two trees in the
forest.  The element that was on top is dropped on the floor and
we're left with a representation of the rest of the list.

And that's that.  When writing your code you want to make sure the
invariants are always true.  Good code should make this fact obvious;
bad code, not so much.

This is a nice little exercise in data structure design and
implementation, and although I @emph{wish} this were really my idea, I
actually got it from reading a book by Chris Okasaki, who has designed
a bunch of these kinds of data structures.  Go forth, and may your
@racket[get] never be slow again.


