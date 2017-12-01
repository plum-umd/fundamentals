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

@title[#:tag "lab07"]{5/29: Constructors and Equality}

The goal of this lab is to practice designing multiple constructors
and overloading the @tt{equals} method.

@lab:section{Finite sets}

It's often useful to represent finite sets of elements, i.e., an
arbitrary number of elements with no order.  There are a couple of
approaches to representing a set.

@itemlist[

@item{One approach is to represent a set just as
you would a list, but to override @tt{equals} to ignore the order and
duplicates when determining if two lists were the same.  This approach
allows for multiple representations of the same information, but uses
@tt{equals} to treat them the same.}

@item{A slight variation of the above is to maintain a list of
elements with no duplicates and have @tt{equals} ignore order.}

@item{Another approach, which is only applicable when the elements can
be ordered in some way, is to maintain a list in sorted order.  This
way there is always a single unique representation of the set and
@tt{equals} can be defined by structural equality.}]

@exercise{Develop an implementation of @tt{Set1<X>} that represents a
set like a list, but overrides @tt{equals} to ignore the order and
duplication of elements.  You must satisfy the contract on @tt{equals}
and @tt{hashCode}.

You should have two constructors: @tt{Set1()} constructs an empty
list.  @tt{Set1(List<X> elems)} constructs a set from the list of
elements.

You should also develop the following methods:

@verbatim|{
// Add given element to this set
Set1<X> add(X elem)

// Subtract given element from this set
Set1<X> sub(X elem)

// Intersect this set with given set
Set1<X> intersect(Set1<X> that)

// Does this set contain given set?
Boolean contains(Set1<X> that)

// Does this set contain given element?
Boolean member(X that)
}|

It may be useful to define your own private constructors.
}

@exercise{Develop an implementation of @tt{Set2<X>} that represents a
set like a list with no duplicates and overrides @tt{equals} to ignore
the order of elements.  You must satisfy the contract on @tt{equals}
and @tt{hashCode} and implement the same constructors and interface as
above (but for @tt{Set2}).}

The last approach relies on an ordering of elements, which is not
always possible, but if we are given an ordering, then elements can be
maintained in sorted order.  For this exercise, we'll use the Java
@tt{Comparator} interface, which is a lot like the @tt{Comparison}
interface we saw in class.  Have a look at the
@link["http://docs.oracle.com/javase/6/docs/api/java/util/Comparator.html"]{docs}
for @tt{Comparator} to get a sense of how objects implementing this
interface work.

@exercise{Develop an implementation of @tt{Set3<X>} that represents a
set as a list of sorted elements.  You should have two constructors:
@tt{Set3(Comparator<X> comp)}, which constructs an empty set using
@tt{comp} as the ordering on elements, and @tt{Set3(Comparator<X>
comp, List<X> elems)} that constructs a set from the list of elements
using the given ordering.  Override @tt{equals} to determine if two
sets are represented by the same sorted lists.  You may assume the two
sets use the same ordering.  You must satisfy the contract on
@tt{equals} and @tt{hashCode} and implement the same interface as
above (but for @tt{Set3}).}

To test the third approach, you should represent sets of @tt{Integer}s
and define your own implementation of @tt{Comparator<Integer>},
perhaps using the @tt{compareTo} method of the @tt{Integer} class.