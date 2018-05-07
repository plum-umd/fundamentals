#lang scribble/manual
@(require "../utils.rkt")

@lecture-title[39]{Drills}

@section{Definitions}

Give a brief definition of
@itemlist[
@item{interface}
@item{class}
@item{object}
@item{abstract class}
@item{iterator}
@item{comparator}
@item{anonymous inner class}
@item{lambda expression}
@item{visitor}
]

@section{Concepts}

Describe two ways in which an abstract class and a class differ.

What does it mean when
@itemlist[
@item{a class implements an interface}
@item{a class extends a class}
@item{an interface extends an interface}
]

@section{Hash code}

If two objects have the same hash code, what can you conclude about
the equality of those objects (assuming their @tt{equals} and
@tt{hashCode} methods are correct)?

@section{Comparisons}

What's the difference between a comparator and comparable object?

@section{Abstract classes}

Describe a situation in which writing an abstract class is justified.

@;ListZipper hash code

@section{Types}

Is this program well-typed?

@verbatim|{
Listof<Integer> is = new Cons <>(5, new Empty<>());
System.out.println(is.first);
}|

Is this program well-typed?

@verbatim|{
Pairof<Integer, Boolean> p = null;
System.out.println(p.left);
}|

@section{Tree traversals}

Suppose you had a binary search tree and you want to produce a list of
elements in the tree in sorted order.  Which traversal do you want?

@itemlist[
@item{NLR}
@item{LNR}
@item{LRN}
]

@section{Computing with iteration}

Implement the following method:

@verbatim|{
// Compute the average of all the numbers in it.
// Assume: it has at least one number
Double avg(Iterable<Double> it) { ... }
}|

@section{Lambda expressions}

Rewrite the following to use lambda expression notation:

@verbatim|{
    Function<Double,Double> dbl = new Function<>() {
        public Double apply(Double d) {
            return d * d;
        }
    };
}|

@section{Methods for lists}

Assume the following interface exists:

@verbatim|{
// Functions X -> Optional<Y>
interface OptFun<X, Y> extends Function<X, Optional<Y>> {}
}|

Design the following methods for @tt{Listof<X>}:

@verbatim|{
    // Produce the list of elements in this list which satisfy p
    Listof<X> filter(Predicate<X> p);

    // Produce the list of results for which f produces something in this list
    <R> Listof<R> filterMap(OptFun<X,R> f);

    // Produce the first element in this list for which p produces true
    Optional<X> findPred(Predicate<X> p);
}|

Show that you can define @tt{filter} in terms of @tt{filterMap}:

@verbatim|{
abstract class AListof<X> implements Listof<X> {
  // Produce the list of elements in this list which satisfy p
  public Listof<X> filter(Predicate<X> p) {
    ...
  }
}
}|

@section{Double dispatch}

When is it justified to use the double dispatch pattern?

Assume the following definitions:

@verbatim|{
interface BT<X> {
  // Produce the tree obtained by following p in this tree    
  BT<X> follow(BTPath p);
}

class Leaf<X> implements BT<X> { ...usual defn...}

class Node<X> implements BT<X> { ...usual defn...}

// Interp: a path through a binary tree
interface BTPath {}

// Interp: end of a path
class End implements BTPath {}

// Interp: go left, followed by path rest
class Left implements BTPath {
    BTPath rest;
    Left(BTPath rest) {
        this.rest = rest;
    }
}

// Interp: go right, followed by path rest
class Right implements BTPath {
    BTPath rest;
    Right(BTPath rest) {
        this.rest = rest;
    }
}
}|

Using the double dispatch pattern, design the @tt{follow} method.  You
may raise an exception if following a path leads you off the end of a
binary tree.


@section{Graphs}

Here is a representation for directed graphs:

@verbatim|{
// A node in a directed graph
class GNode {
    String name;
    Listof<GNode> neighbors;

    GNode(String name) {
        this.name = name;
        this.neighbors = new Empty<>();
    }

    // EFFECT: add the given node to this node's neighbor
    public void addNeighbor(GNode n) {
        this.neighbors = new Cons<>(n, this.neighbors);
    }

    // Is there a path from this node to the given node?
    public Boolean hasPath(GNode to) { ... }
}
}|

Design the @tt{hasPath} method.  Here some examples:

@verbatim|{
    void testGraph(Tester t) {
        // +---------+
        // v         |
        // A--->B--->C-->D
        //      +--->E-->F
        //
        GNode a = new GNode("A");
        GNode b = new GNode("B");
        GNode c = new GNode("C");
        GNode d = new GNode("D");
        GNode e = new GNode("E");
        GNode f = new GNode("F");

        a.addNeighbor(b);
        b.addNeighbor(c);
        c.addNeighbor(d);
        b.addNeighbor(e);
        e.addNeighbor(f);
        c.addNeighbor(a);

        t.checkExpect(a.hasPath(a), true);
        t.checkExpect(a.hasPath(b), true);
        t.checkExpect(a.hasPath(e), true);
        t.checkExpect(e.hasPath(d), false);
    }
}|

@section{Visitors}

Recall the following method for @tt{Listof<X>}:

@verbatim|{
    // Is this list sorted in ascending order according to c?
    Boolean isSorted(Comparator<X> c);
}|

It can be implemented with an accumulator helper method that
remembers the prior element in the list.

Suppose this method was not available for @tt{Listof<X>} and you
couldn't make any further changes to @tt{Listof<X>}.  Design a visitor
that computes the same thing as this method.
