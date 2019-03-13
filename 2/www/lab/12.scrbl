#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")
@(require "../utils.rkt")

@lab-title[12]{Dispatching Visitors}

@section[#:style 'unnumbered #:tag "lab12:intro"]{Intro}

You'll work in this lab with ad-hoc partners.

The two of you will work as a team to solve problems. At any time, one
of you will be the @bold{Head} and the other will be the
@bold{Hands}. The @bold{Head} does the thinking and the @bold{Hands}
does the typing. @bold{Hands} type only what the @bold{Head} tells
them to, but you're free to discuss any issues that pop up. You should
switch off during the lab to make sure each of you get practice
problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

You should start this lab with @link["Lab13.zip"]{this project
skeleton}. Unzip the file into your IdeaProjects directory and open it
with IntelliJ to get started.

@section[#:style 'unnumbered #:tag "lab12:case"]{Recall}

Operations on union data definitions often require case analysis. Last
semester, we handled all cases in one place. For example:

@isl-block{
;; A [Listof X] is one of:
;; - '()
;; - (cons X [Listof X])

(define (list-template l)
  (cond [(empty? l) ...]
        [(cons? l) (... (first l) ...
                        (list-template (rest l)))]))

;; list-sum : [Listof Number] -> [Listof Number]
;; Sum the elements of the given list of numbers.
(check-expect (list-sum '()) 0)
(check-expect (list-sum '(1 2 3 4)) 10)
(define (list-sum l)
  (cond [(empty? l) 0]
        [(cons? l) (+ (first l) (list-sum (rest l)))]))
}

If we want to add more operations we just write more functions that
perform case analysis on lists.

In object-oriented programming we've handled case analysis with
methods in each of the implementing classes of some interface.

@verbatim|{
interface IList {
  Integer sum();
}

class IntEmpty implements IList {
  public Integer sum() { return 0; }
}

class IntCons implements IList {
  Integer first;
  IList rest;

  IntCons(Integer first, IList rest) {
    this.first = first;
    this.rest = rest;
  }

  public Integer sum() {
    return this.first + this.rest.sum();
  }
}
...
IList il0 = new IntEmpty();
IList il1 = new IntCons(1, il0);
IList il2 = new IntCons(2, il1);
IList il3 = new IntCons(3, il2);
IList il4 = new IntCons(4, il3);

boolean testIListSum(Tester t) {
  return t.checkExpect(il0.sum(), 0)
          && t.checkExpect(il4.sum(), 10);
}
}|

If we want to implement new operations on @tt{IList}s, we would add a
method signature to the interface and implement that method in both
the @tt{IntEmpty} and @tt{IntCons} classes. Easy.


@section[#:style 'unnumbered #:tag "lab12:problem"]{The Problem}

How do we add operations to a library that we can't modify?

We're all tired of writing the same old @tt{List} interface and
@tt{Cons}, @tt{Empty} classes. We should just write one and make it
into a library that we can use forever and never touch again.

We can't always predict which operations we'll need on @tt{List}s. In
this lab you'll learn the @emph{visitor pattern}: a general
way to add arbitrary operations to a fixed set of related classes
(e.g. @tt{Empty} and @tt{Cons}).


@section[#:style 'unnumbered #:tag "lab12:solution"]{The Solution}

We have learned how to implement case analysis for classes using
@emph{double-dispatch}. To solve our operation extension woes, we'll
use the same trick in a more general form.

Any operation on lists must handle empty and non-empty lists. The
@tt{ListVisitor} interface has a @tt{visit} method for each branch of
the union on which we're operating.

@verbatim|{
interface ListVisitor<T, U> {
  U visitEmpty();                      // Handle empty lists
  U visitCons(T first, List<T> rest);  // Handle non-empty lists
}
}|

@bold{Note}: The type parameter @tt{T} is the type of the elements of
the list; @tt{U} is the return type of the operation.

The @tt{ListVisitor} interface handles the case analysis by visiting
lists. The classes implementing the @tt{List} interface only need to
know how to @tt{accept} the visitor and dispatch to the proper method.

@verbatim|{
interface List<T> {
  <U> U accept(ListVisitor<T, U> visitor);
}

class Empty<T> implements List<T> {
  public <U> U accept(ListVisitor<T, U> visitor) {
    return visitor.visitEmpty();
  }
}

class Cons<T> implements List<T> {
  T first;
  List<T> rest;

  Cons(T first, List<T> rest) {
    this.first = first;
    this.rest  = rest;
  }

  public <U> U accept(ListVisitor<T, U> visitor) {
    return visitor.visitCons(this.first, this.rest);
  }
}
}|

We can easily implement list-sum as a @tt{ListVisitor}.

@verbatim|{
class ListSum implements ListVisitor<Integer, Integer> {

  public Integer visitEmpty() {
    return 0;
  }

  public Integer visitCons(Integer first, List<Integer> rest) {
    return first + rest.accept(this);
  }

}
}|

When @tt{ListSum} visits an @tt{Empty} list it returns 0; when it visits
a @tt{Cons} list it returns the sum of the first recursive result of
the rest accepting the @tt{ListSum} visitor.


@section[#:style 'unnumbered #:tag "lab12:listops"]{List operations}

You're going to design a number of simple list operations as
@tt{ListVisitor}s.

@bold{Ex 1}: Design @tt{ListLength} as a @tt{ListVisitor}. It should
work on lists with elements of any type @tt{T} and return the
@tt{Integer} length of the list.

@bold{Ex 2}: Design @tt{ListAppend} as a @tt{ListVisitor}. It should
work on lists with elements of any type @tt{T} and return the new
@tt{List<T>}.

@bold{Hint}: List append requires an argument: the list suffix to be
added to the end of the list, but we can't modify the visiting
methods' signatures. Try making a @tt{ListAppend} constructor that
accepts the suffix and holds on to it in a field until it gets used in
@tt{visitEmpty}.

@bold{Ex 3}: Design @tt{ListNth} as a @tt{ListVisitor}. It should work
on lists with elements of any type @tt{T} and return the @tt{n}th
element of the list, if it exists, and throw a runtime exception
otherwise.

@bold{Hint}: This also requires a constructor argument: the index
@tt{n}. You may also have to create a new @tt{ListNth} visitor in the
recursive @tt{accept} to decrement the index as you walk the list.

@bold{Ex 4}: Design @tt{ListReverse} as a @tt{ListVisitor}. It should
work on lists with elements of any type @tt{T} and return the reversed
@tt{List<T>} as the result. You may either use @tt{ListAppend} in your
implementation or give @tt{ListReverse} a constructor with an
accumulating argument.


@section[#:style 'unnumbered #:tag "lab12:btvisitor"]{Visiting binary trees}

Now that we've had some practice writing visiting operations on
@tt{List}s, you're ready to implement your own visitor pattern for
another interface and implementing classes. Here is a simple
definition of generic binary trees.

@verbatim|{
interface BT<T> {}

class Leaf<T> implements BT<T> {}

class Node<T> implements BT<T> {
  T value;
  BT<T> left;
  BT<T> right;

  Node(T value, BT<T> left, BT<T> right) {
    this.value = value;
    this.left = left;
    this.right = right;
  }
}
}|

@bold{Ex 5}: Implement an interface @tt{BTVisitor} for visiting
operations on binary trees. It should have two type parameters: one
for the type the tree elements and another for the result type of the
operation.

@bold{Ex 6}: Add an @tt{accept} method to the @tt{BT} interface and
implementing classes that accepts @tt{BTVisitor} operations with
arbitrary return types.

@bold{Hint}: Take another look at the @tt{List} @tt{accept} signature
and methods if you run into issues.


@section[#:style 'unnumbered #:tag "lab12:btops"]{Binary tree operations}

@bold{Ex 7}: Design @tt{BTSum} as a @tt{BTVisitor}. It should work on
binary trees of @tt{Integer}s and return the @tt{Integer} sum of all
the values in the tree.

@bold{Ex 8}: Design @tt{BTDepth} as a @tt{BTVisitor}. It should work
on trees with values of any type @tt{T} and return the @tt{Integer}
depth of the tree.

@bold{Ex 9}: Design @tt{BTMirror} as a @tt{BTVisitor}. It should work
on trees with values of any type @tt{T} and return the mirrored
@tt{BT<T>} as the result.

@bold{Ex 10}: Design @tt{BTInOrder} as a @tt{BTVisitor}. It should
work on trees with values of any type @tt{T} and return a @tt{List<T>}
of the
@link["https://en.wikipedia.org/wiki/Tree_traversal#In-order"]{in-order
traversal} of the tree. This means take the values in the left branch
first, then this node's value, then the right branch's values.

For example:
@verbatim|{
//         4
//        / \
//       2          =>   '(1 2 3 4)
//      / \
//     1   3
//    / \ / \
}|

@section[#:style 'unnumbered #:tag "lab12:submit"]{Submission}

Submit a zip file of your work at the end of lab.
