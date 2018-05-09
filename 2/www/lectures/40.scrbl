#lang scribble/manual
@(require "../utils.rkt")

@lecture-title[40]{Drill Solutions}

@section{Definitions}

Give a brief definition of
@itemlist[
@item{interface

Answer: definition for a set of objects described by method signatures
objects in the set must implement.

}

@item{class

Answer: definition for a set of objects described by fields and
methods objects in the set have and a procedure for consucting such
objects.

}
@item{object

Answer: a value with fields and methods.
}

@item{abstract class

Answer: a class definition which does not allow for instances to be constructed directly.
}

@item{iterator

Answer: a mutable object that represents an iteration of a set of values.
}

@item{comparator

Answer: an object that represents a procedure for determining the relative order of two objects.
}

@item{anonymous inner class

Answer: a short-hand for locally defining an instance of a class without naming them class.
}
@item{lambda expression

Answer: a short-hand for locally defining an instance of a class that
has one method without naming the class or method.

}
@item{visitor

Answer: an object that represents a computation over the value it visits.}
]

@section{Concepts}

Describe two ways in which an abstract class and a class differ.

Answer: abstract classes cannot be constructed (directly).  Abstract
classes do not need to implement all the methods of the interfaces
they implement.

What does it mean when
@itemlist[
@item{a class implements an interface

Answer: the class implements the method signatures defined in the interface.}
@item{a class extends a class

Answer: the class inherits the fields, methods, and constructors of the class it extends.}
@item{an interface extends an interface

Answer: the interface includes all of the signatures of the interface it extends.}
]

@section{Hash code}

If two objects have the same hash code, what can you conclude about
the equality of those objects (assuming their @tt{equals} and
@tt{hashCode} methods are correct)?

Answer: nothing.

@section{Comparisons}

What's the difference between a comparator and comparable object?

Answer: a comparable object implements a {\tt compareTo} method that
compares it with a given object; a comparator object implements a {\tt
compare} method that compares two given objects (and is thus
independent of the objects being compared).

@section{Abstract classes}

Describe a situation in which writing an abstract class is justified.

Answer: two classes which implement the same interface have some
methods that are identical.

@;ListZipper hash code

@section{Types}

Is this program well-typed?

@verbatim|{
Listof<Integer> is = new Cons <>(5, new Empty<>());
System.out.println(is.first);
}|

Is this program well-typed?

Answer: no. (Even though it would run without error.)

@verbatim|{
Pairof<Integer, Boolean> p = null;
System.out.println(p.left);
}|

Answer: yes. (Even though it will crash when run.)

@section{Tree traversals}

Suppose you had a binary search tree and you want to produce a list of
elements in the tree in sorted order.  Which traversal do you want?

@itemlist[
@item{NLR}
@item{LNR}
@item{LRN}
]

Answer: LNR.

@section{Computing with iteration}

Implement the following method:

@verbatim|{
// Compute the average of all the numbers in it.
// Assume: it has at least one number
Double avg(Iterable<Double> it) { ... }
}|


Answer:

@verbatim|{

// Compute the average of all the numbers in it.
// Assume: it has at least one number
Double avg(Iterable<Double> it) { 
  Inteter ct = 0;
  Double sum = 0.0;
  for (Double d : it) {
    ct = ct + 1;
    sum = sum + d;
  }
  return sum / ct;
}

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

Answer:

@verbatim|{
    Function<Double,Double> dbl = d -> d * d;
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

Answer:

@verbatim|{
// In Empty

    public <R> Listof<R> filter(Predicate<X> p) {
        return new Empty<>();
    }
 
    public <R> Listof<R> filterMap(OptFun<X, R> f) {
        return new Empty<>();
    }

    public <R> Optional<R> findPred(Predicate<X> p) {
        return Optional.empty();
    }

// In Cons

    public <R> Listof<R> filter(Predicate<X> p) {
        return p.test(this.first) ?
               new Cons<>(this.first, this.rest.filter(p)) :
               this.rest.filter(p);
    }
 
    public <R> Listof<R> filterMap(OptFun<X, R> f) {
        Optional<X> r = f.apply(this.first);
        return r.isPresent() ?
               new Cons<>(r.get(), this.rest.filterMap(f)) :
               this.rest.filterMap(f);
    }

    public <R> Optional<R> findPred(Predicate<X> p) {
        return p.test(this.first) ?
               Optional.of(this.first) :
               this.rest.findPred(p);
    }
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

Answer:

@verbatim|{
// In AListof

    public Listof<X> filter(Predicate<X> p) {
        return this.filterMap(x -> p.test(x) ? 
               Optional.of(x) : 
               Optional.empty());
    }
}|

@section{Double dispatch}

When is it justified to use the double dispatch pattern?

Answer: when computing something in terms of two values belonging to
union data definitions.

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


Answer:

@verbatim|{

// In Leaf

    public BT<T> follow(BTPath p) {
        return p.followLeaf(this);
    }

// In Node

    public BT<T> follow(BTPath p) {
        return p.followNode(this);
    }


// Interp: a path through a binary tree
interface BTPath {
    <X> BT<X> followLeaf(Leaf<X> l);
    <X> BT<X> followNode(Node<X> l);
}

// Interp: end of a path
class End implements BTPath {
    public <X> BT<X> followLeaf(Leaf<X> l) {
        return l;
    }

    public <X> BT<X> followNode(Node<X> n) {
        return n;
    }
}

// Interp: go left, followed by path rest
class Left implements BTPath {
    BTPath rest;
    Left(BTPath rest) {
        this.rest = rest;
    }

    public <X> BT<X> followLeaf(Leaf<X> l) {
        throw new RuntimeException("ran off end");
    }

    public <X> BT<X> followNode(Node<X> n) {
        return n.left.follow(this.rest);
    }
}

// Interp: go right, followed by path rest
class Right implements BTPath {
    BTPath rest;
    Right(BTPath rest) {
        this.rest = rest;
    }

    public <X> BT<X> followLeaf(Leaf<X> l) {
        throw new RuntimeException("ran off end");
    }

    public <X> BT<X> followNode(Node<X> n) {
        return n.right.follow(this.rest);
    }
}

}|

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

Answer:

@verbatim|{
    // Is there a path from this node to given node?
    public Boolean hasPath(GNode to) {
        return this.hasPathAcc(to, new Empty<>());
    }

    public Boolean hasPathAcc(GNode to, Listof<GNode> seen) {
        return !seen.exists(n -> n.name.equals(this.name)) &&
                this.equals(to) ||
                this.neighbors.exists(n ->
                        n.hasPathAcc(to, new Cons<>(this, seen)));
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

@verbatim|{
class IsSorted<X> implements ListVisitor<X, Boolean> {
    Comparator<X> c;

    IsSorted(Comparator<X> c) {
        this.c = c;
    }

    public Boolean visitEmpty(Empty<X> e) {
        return true;
    }

    public Boolean visitCons(Cons<X> c) {
        return c.rest.accept(new IsSortedAcc(c.first));
    }

    class IsSortedAcc implements ListVisitor<X, Boolean> {
        X prior;

        IsSortedAcc(X prior) {
            this.prior = prior;
        }

        public Boolean visitEmpty(Empty<X> e) {
            return true;
        }

        public Boolean visitCons(Cons<X> cons) {
            return c.compare(prior, cons.first) <= 0 &&
                    cons.rest.accept(new IsSortedAcc(cons.first));
        }
    }
}
}|
