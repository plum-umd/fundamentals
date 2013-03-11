#lang scribble/manual

@(require "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt"
          racket/runtime-path)

@(define-runtime-path skip-list "skip-list.png")

@(define exercise (exercise-counter))

@title[#:tag "lab10"]{3/11: Skip lists}

@lab:section{Imperative lists}

If you remember back to lecture, we've started looking at new ways of
building data structures that use @emph{mutation}.  The main goal of
this lab will be to develop a new kind of data structure, but first we
will need to define a new kind of list that is mutable.  (It will come
in handy later.)

Here's an interface (in comments) for a @emph{mutable list}, which
lets you modify a list with the @tt{set} method.  The @tt{set} method
takes an index and an element and destructively updates the list to
contain the given element at the given index.  We've also included a
@tt{ref} method that produces the element at a given index:

@indented{@verbatim|{
  // A List<V> implements
  //
  // ref : Integer -> V
  // returns the element at the given index
  //
  // set : Integer V -> Void
  // Effect: updates the element in-place
  // sets the element at the given index
}|}

@exercise{
  Write down a data definition that implements the above interface.
}

@exercise{
  Implement the data defintion you wrote down and implement both of
  the @racket[ref] and @racket[set] methods.
}

@lab:section{Another day, another data structure}

There are many ways for storing a sorted collection of data, such as
sorted lists and balanced search trees.  Both of these implement the
interface of an ordered (or sorted) set. However, both of these data
structures have their own issues.

With sorted lists, insertion and search of an element of the set takes
time proportional to the entire length of the list in the worst case since
you can end up traversing the whole list.

Binary search trees are much better in that these operations are both
logarithmic in the number of elements. On the other hand, they are more
complicated than lists and can take up a significant amount of memory
on your computer.

Could we do even better? It turns out you can, for some definition of
``better''.  We will look at a data structure called a @emph{skip
list} that uses randomization to provide the same @emph{expected}
running time as a binary search tree, but using a data structure that
is simpler and potentially uses less memory ("expected" here means
that over a large number of trials we will get that running time on
average).

@lab:section{Building skip lists}

A skip list is essentially a stack of linked lists that are constructed in a
specific way. The stack has some fixed maximum level (let's say @racket[5] for
this lab).

Each element in the skip list will participate in some number of the linked
lists (up to the max level). For each element you want to add, you flip a coin
for each level and the number of heads tells you how many of the lists this
element will be included in. The effect of this is that elements are less
likely to show up in higher levels of the skip list and more likely to show up
at the bottom. The one exception is that all elements are in the bottom list,
so that the bottom list is a normal linked list.

Here is a picture from Wikipedia of what a skip list might look like:

@image[skip-list]

We will implement a variant of a skip list that is never empty, so the
constructor takes an initial key and value to start out with. This makes the
implementation a little bit simpler (there is no "header" from that picture).
The initial element will show up at all levels.

@indented{@verbatim|{
  // A SkipList<V> is a
  //   new SkipList<V>(Integer, V)
  // and implements
  //
  // insert : Integer V -> Void
  // Effect: updates the list with a new element
  // add an element with the given integer key
  //
  // search : Integer -> V
  // look up the element matching the given key
}|}

The @racket[new SkipList<V>(n, v)] expression creates a new skip list with a
single element. Notice that the contract for @racket[insert] states that the
result is @racket[Void].  We are going to implement this skip list using
mutation because it is a data structure that is easier to implement in this
style.

To implement the skip list, we will use an auxillary data definition.

@indented{@verbatim|{
  // A Node<V> is one of
  //   new MTNode()
  //   new Element(Integer, V, Integer)
  // and implements
  //
  // isEmpty : -> Boolean
  // specify whether this is the end
  //
  // getKey : -> Integer
  // get the key at this node
  //
  // getValue : -> V
  // get the value at this node
  //
  // setValue : Integer V -> Void
  // Effect: updates the value in-place
  // sets the value for this node
  //
  // getNext : Integer -> Node<V>
  // gets the next node linked at the given level
  //
  // setNext : Integer Node<V> -> Void
  // Effect: updates the next node in-place
  // sets the next node linked at the given level
}|}

A @racket[Node] is a data definition and interface for the elements in the
list. The end of the list is marked by @racket[MTNode] while the elements
in between are @racket[Element]s. Since @racket[MTNode] shouldn't do anything,
you can just implement stubs for its methods or return errors.

For each @racket[Element], you should use the @racket[List] implementation that
you defined earlier to track the nodes linked to from this node. There will be
a link for every level that this node is contained in. Its constructor takes
the initial key and value (@racket[Integer] and @racket[V] types respectively)
and also the height of this node.

The algorithm for the @racket[search] method is easier, so we will start with
that.

To search a skip list, start at the very top level and keep going right.
When you either hit the end (an @racket[MTNode]) or you hit a key that is
bigger than the one you're looking for, you go down a level. It will probably
be helpful to write this as a helper function that recurs on the level of the
skip list.

For example, if you are looking for the key @racket[4] in the example
skip list in the picture above, you would start at the top and immediately
see that you end up at the end. Then you start at the second level from the
top and find a node with @racket[4], so you're done.

If you were looking for @racket[5], you would go to @racket[4] and then find
that the next node is @racket[6], so you go down again. Since the next node
is still @racket[6], you will go down again. Finally, the next node is
@racket[5] at the bottom row so you're done (now you might see why it's called
a "skip" list).

@exercise{
  Implement the @racket[search] method and test it with a one-element
  skip list.
}

For insertion, the code is very similar. However, this time you have to keep
track of an extra thing. Every time you go down a level while searching, you
have to record the links that you need to update when you add the new node.

If the node with the key already exists in the tree, just find it and then
update its value.

If the node does not yet exist, use the same technique you used to search
through the tree, but keep storing the right-most node that you looked at in a
list. Then build a new node. For the new node, you will want to randomly
flip a coin up to @racket[maxLevel - 1] (so 4) times. When you see the first
tails, the number of heads so far plus one is the height of the new node (this means
that taller nodes are less likely).

At the very end, use @racket[setNext] to set the next node for the new
node to the next nodes for the nodes you kept track of in your list.  Then
update their next elements to be the new node that you are adding into the
list. You @emph{only} need to update the next links up to the height of the
new node.

@exercise{
  Implement the @racket[insert] method. Now you can write better tests
  for the @racket[search] method using @racket[insert] to build bigger
  skip lists.
}

Now that you've implemented the basic functionality, try to think of how
you would implement a delete method. First of all, what should its contract
be?

@exercise{
  Write down the changes you need to your interface to support @racket[delete].

  Now implement the method. Its structure should be very similar to @racket[insert].
}
