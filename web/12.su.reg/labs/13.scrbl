#lang scribble/manual

@(require "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt"
          scribble/eval)

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require lang/htdp-intermediate))
    the-eval))


@(define exercise (exercise-counter))

@title[#:tag "lab13"]{6/19: Properties of Quick Lists}

The goal of this lab is to make practice developing testable
properties.

@lab:section{Properties of Quick Lists}

Start by writing some properties that should hold for all quick lists.
For example, the fundamental law of @tt{first} and @tt{cons}:

@verbatim{
∀ ls : QList<X> . ∀ x : X . ls.cons(x).first().equals(x)
}

And @tt{rest} and @tt{cons}:

@verbatim{
∀ ls : QList<X> . ∀ x : X . ls.cons(x).rest().equals(ls)
}

@exercise{Implement the above properties as functions on lists and
elements.  Test them against your quick list implementation.}

Did you find a counter-example to the second property?  Why?

Because quick lists are immutable, they should really obey structural
equality rather than intensional equality.  Thus two lists are equal
if they contain equal elements in the same order.

@exercise{Override @tt{equals} and @tt{hashCode} for quick lists so
that @tt{equals} implements structural equality.}

Can you find a counter-example to the properties now?  (If so, you
may have a bug.)

We can formulate properties about other methods now, too.  For
example, there's a property for @tt{get} and @tt{cons} analogous to
the above:

@verbatim{
∀ ls : QList<X> . ∀ x : X . ls.cons(x).get(0).equals(x)
}

[The above property used to have @tt{ls.size()} in place of @tt{0},
which is clearly wrong and has been fixed.]

@exercise{Implement the @tt{get} property as a function on lists and
elements.  Test it against your quick list implementation.}

And here's one for @tt{size} and @tt{cons}:

@verbatim{
∀ ls : QList<X> . ∀ x : X . ls.cons(x).size().equals(ls.size()+1)
}

@exercise{Implement the @tt{size} property as a function on lists and
elements.  Test it against your quick list implementation.}


@lab:section{Properties of Quick List Implementations}

The above properties are really properties of lists in general and not
specific to quick lists.  But quick lists have important invariants
and we can use property-based testing to see if those invariants are
maintained.  For example, a quick list is a forest of @emph{full}
binary trees.  Your code should be written to assume this for inputs
and guarantee it for outputs, but now we can actually test that there
were no mistakes:

@tt{∀ ls : QList<X> .}
``each tree in @tt{ls} is full''

The property is in English because giving it in more detail would
require knowing about how you represented quick lists.

Recall that a tree is full if its left and right tree are of the same
size.

@exercise{Implement the fullness property as a function on lists.
Test it against your quick list implementation.}

You should be careful in coding the above since it likely relies on
@tt{size}, which is probably written to assume the invariant you are
testing.  One way to avoid this is to add a @tt{slowSize} method that
calculates the size of a tree without assuming it is full.

The fullness property still doesn't quite characterize the real
invariant of quick lists, which is that the forest is in strictly
ascending order of size with the possible exception of the first two,
which may be the same size:

@tt{∀ ls : QList<X> .}  ``Either the first two trees of @tt{ls} are of
equal size and every subsequent tree is strictly larger, or every tree
is strictly larger''

@exercise{Implement the larger property as a function on lists.  Test
it against your quick list implementation.}

Finally, these properties are most usefully tested against
@emph{random} lists.  To generate random lists of integers, here's a
useful utility (this code wasn't tested, so you may have to touch it
up a bit):

@verbatim|{
// Generator for random lists of at most given size
// with elements drawn from integers in given range.
class RandomList {
   Integer size;  // Size of list
   Integer max;   // Maximum element value
   Random r = new Random();
   RandomList(Integer size, Integer max) {
      this.size = size;
      this.max = max;
   }

   // Produce a random list of integers
   QList<Integer> nextList() {
      QList<Integer> ls = new QEmpty<Integer>();
      Integer s = r.nextInt() % size;
      for (Integer i = 0; i < s; i = i+1) {
        ls = ls.cons(r.nextInt() % max);
      }
      return ls;
   }
}
}|