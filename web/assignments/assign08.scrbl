#lang scribble/manual
@(require "../utils.rkt"
	  "../unnumbered.rkt"
          (for-label (except-in class/2 empty cons first rest list-ref length e check-expect))
          (for-label (only-in lang/htdp-intermediate-lambda check-expect))
	  (for-label class/universe))

@title[#:tag "assign09"]{3/14: Java}

Due: 3/14.  Language: Java.

@section{Fundamentals reprise}

Solve problems 2, 4, 6, and 9 from the
@seclink["assign05"]{Fundamentals assignment}, in Java. As before,
problem 6 is a revision of problem 4; you should turn in one solution
that solves both parts.

@section{Binary search trees}

Here is the class diagram for binary trees parameterized over the type of their elements:

@verbatim{
          +-------------+
          |  IBT<T>     |<----------+
          +-------------+<--------+ |
            /_\   /_\             | |
             |     |              | |
+--------------+  +-----------+   | |
| Node<T>      |  | Leaf<T>   |   | |
+--------------+  +-----------+   | |
| T val        |                  | |
| IBT<T> left  |------------------+ |
| IBT<T> right |--------------------+
+--------------+
}

(This is a slightly different representation of binary trees than
you saw in class. The important point is that a Leaf has no data,
which let's us represent empty binary trees, as well trees with only
two values; neither of which is possible if a Leaf contains a value.)

Give class and interface definitions for the diagram above, then
implement the visitor pattern for binary trees. Visitors should be
parameteric in both the type of elements in the tree they visit and
the type of result they compute.

Design the following two implementations of a binary tree visitor:

@itemlist[
    @item{@tt{LeftMost} : produces the leftmost value of the tree it
    visits (if there is one). If there is no leftmost value (for
    example, if the tree is just a Leaf), the visitor should signal a
    run-time exception.}

    @item{@tt{RightMost} : produces the rightmost value of the tree it
    visits (if there is one). If there is no rightmost value (for
    example, if the tree is just a Leaf), the visitor should signal a
    run-time exception. }]

It may be useful to design two helper visitors, @tt{LeftMostAcc} and
@tt{RightMostAcc}, which accumulate the leftmost (or rightmost)
element seen so far.

Binary trees are particularly useful when their elements are sorted
according to some ordering. For example, you can find out if a given
element is contained in a sorted binary tree must faster than you in
one which is not. Of course, whether a given binary tree is sorted or
not depends on the ordering of elements we have in mind. For example,
recalling our discussion of the Boston Marathon, a binary tree of
runners that is sorted by runners' finish times is probably not the
same as if we sorted by runners' bib number. So the question "is the
tree sorted?" depends on the ordering we'd like to impose on the
elements.

Since ordering elements is such a fundamental operation, and since
orders exist independently of elements (for example runners may be
ordered by bib number, finish time, age, etc.), Java has an interface
for comparisons defined by the
@link["http://docs.oracle.com/javase/6/docs/api/java/util/Comparator.html"]{@tt{Comparator<T>}}
interface. A @tt{Comparator<T>} which are represented by a functional
object that has a method:

@verbatim{
  // Produces a negative integer if t1 is "less than" t2.
  // Produces a positive integer if t1 is "greater than" t2.
  // Produces zero if t1 is "equal to" t2.
  int compare(T t1, T t2);
}

Using the following representation of runners:

@verbatim{
// Represents a runner with name, age (in years), bib number, and time
// (in minutes).
class Runner {
    String name;
    Integer age;
    Integer bib;
    Integer time;
    Runner(String name, Integer age, Integer bib, Integer time) {
        this.name = name;
        this.age = age;
        this.bib = bib;
        this.time = time;
    }
}
}

design a @tt{Comparator<Runner>} that orders runners by increasing
age; design a @tt{Comparator<Runner>} that orders runners by
decreasing bib number.

Design a @tt{Comparator<String>} that orders strings by reverse
alphabetic order.

Now design a binary tree visitor @tt{IsSorted<T>} that is given a
@tt{Comparator<T>} when constructed, and uses it to determine if the
binary tree it visits is sorted or not according to the ordering of
the comparator.

This discussion suggests that a binary search tree is just a binary
tree that is sorted according to some comparison. Design a
representation of binary search trees according to the following class
diagram:

@verbatim{
  +-----------+
  | IBST<T>   |
  +-----------+
      /_\
       |
+--------------------+
| BST<T>             |
+--------------------+
| Comparator<T> comp |
| IBT<T> bt          |
+--------------------+
}

The @tt{bt} field should contain a binary tree that we assume is
sorted acording to the @tt{comp} ordering.

The @tt{IBST<T>} interface should include the following methods:

@verbatim{
// Insert given element into this binary search tree.
// (The resulting tree must be sorted.)
IBST<T> insert(T elem);

// Get the smallest element in this binary search tree.
// (Raise a run-time exception if there is no such element.)
T min();

// Get the largest element in this binary search tree.
// (Raise a run-time exception if there is no such element.)
T max();
}

In addition to the above methods, @tt{IBST<T>} should also implement
the @tt{IBT<T>} interface, since after all, a binary search tree is
just a kind of binary tree, and thus should implement all the behavior
of binary trees in addition to that of binary search trees. Finally,
the fact that the bt field is assumed to be sorted is not a valid
assumption if users of the @tt{BST<T>} class can construct a BST with
an arbitrary @tt{IBT<T>} and @tt{Comparator<T>}. Redesign the
@tt{IBST<T>} class to ensure the integrity of the data it
contains. That is, have the public constructor for @tt{IBST<T>} check
that the given binary tree is sorted according to comp. If it isn't,
you can either raise an exception, or sort the given tree. In either
case, it should be impossible to construct a binary search tree that
contains a binary tree that is not sorted.

To the extent possible, your code should re-use functionality
developed earlier in this problem.

@section{Trie, trie again}

Van Horn spent a bunch of time talking about Java in lab on Monday, so
there wasn't much time to finish the lab. So you'll do that in
homework.  

Implement the @tt{Trie<V>} interface, including the @tt{size} and
@tt{matchPrefix} methods.  The details of tries, as well as the
required methods, can be found in @elemref["lab-trie"]{the lab
instructions}.