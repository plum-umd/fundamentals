#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner 2htdp/universe) "helper.rkt")
@(require "../utils.rkt")

@lab-title[14]{Cyclic Lists}

@section[#:style 'unnumbered #:tag "lab14:intro"]{Intro}

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

You can start this lab with @link["Lab16.zip"]{this project
skeleton}. In it you'll find an IntelliJ project with two Java files:
@tt{Listof.java} and @tt{Lab16.java}. You'll be editing both files
during this lab.


@section[#:style 'unnumbered #:tag "lab14:recall"]{Recall}

We've been learning about mutable and cyclic data structures in
lecture. In this lab we're going to focus on writing operations on
cyclic lists.

Here's an example of a standard singly-linked list:

@verbatim|{
+-----------+      +-----------+      +-----------+      +----+
| first = 1 |   -->| first = 2 |   -->| first = 3 |   -->| Mt |
| rest------+--/   | rest------+--/   | rest------+--/   +----+
+-----------+      +-----------+      +-----------+
}|

The rest field points to the next cell in the list, which eventually
reaches its way to an empty list. Let's see what it takes to handle
operations on lists that look like the following:

@verbatim|{
+-----------+      +-----------+      +-----------+
| first = 1 |   -->| first = 2 |   -->| first = 3 |
| rest------+--/   | rest------+--/   | rest------+--\
+-----------+      +-----------+      +-----------+   |
      ^                                              /
       \                                            /
        -------------------------------------------/
}|


@section[#:style 'unnumbered #:tag "lab14:aside"]{First, A Note on Equality}

Equality is a subtle subject in computer science. Most of you have
probably noticed that Java includes two main equality operations: the
@tt{equals} method and the operator @tt{==}. These don't always act as
you would expect or hope.

@bold{Ex 1}: Do you expect this test to pass or fail? Uncomment this
test in @tt{Lab14.java} after you've talked it over with your partner.

@verbatim|{
Integer one = 1;
Integer otherOne = 1;
t.checkExpect(one == otherOne, ???);
}|

@bold{Ex 2}: What about this test?

@verbatim|{
Integer oneThousand = 1000;
Integer otherOneThousand = 1000;
// t.checkExpect(oneThousand == otherOneThousand, ???);
}|

The salient detail here is that the operator @tt{==} tests for
@emph{referential equality}. It tells you whether two objects are
identical in memory. Java caches small ([-128, 127]) integer objects,
which is why @tt{(1 == 1)} but @tt{(1000 != 1000)}.

Knowing whether two objects are referentially equal can be useful, for
example, when trying to identify a cycle in a list.


@section[#:style 'unnumbered #:tag "lab14:iscyclic"]{Identifying Cycles}

The list implementation in @tt{Listof.java} implements a few
operations that work fine on regular lists. However, these operations
will loop forever if we try to apply them to cyclic lists.

@bold{Ex 3}: Uncomment and run the four tests on the cyclic list in
@tt{Lab14.java} to confirm they loop forever.

In @tt{Listof.java} you'll see the already implemented methods
@tt{isCyclic} and @tt{isCyclicHelper}.

@verbatim|{
// In the interface:
// Is this list a cyclic list?
Boolean isCyclic();
Boolean isCyclicHelper(Cons<X> head);

// In Mt:
public Boolean isCyclic() { return false; }
public Boolean isCyclicHelper(Cons<X> head) { return false; }

// In Cons:
public Boolean isCyclic() {
    return rest.isCyclicHelper(this);
}

public Boolean isCyclicHelper(Cons<X> head) {
    if (head == this) {
        return true;
    } else {
        return this.rest.isCyclicHelper(head);
    }
}
}|

Note that if a list contains an empty list, it cannot contain a
cycle. In the @tt{Cons} case, the method @tt{isCyclic} passes
@tt{isCyclicHelper} the head of the list, and we recur. If we find the
head of the list again, we know we have a cyclic list.


@section[#:style 'unnumbered #:tag "lab14:ops"]{Operations on Cyclic Lists}

Each of these operations already work on non-cyclic lists. Extend them
with a helper method that maintains a pointer to the head of the list
like @tt{isCyclic} to make them work on cyclic lists as well.

@bold{Ex 4}: Implement the method @tt{length} using @tt{lengthHelper} to
ensure termination for cyclic lists.

@bold{Ex 5}: Implement the method @tt{contains} using @tt{containsHelper} to
ensure termination for cyclic lists.

@bold{Ex 6}: Implement the method @tt{foldr} using @tt{foldrHelper} to
ensure termination for cyclic lists.

@bold{Ex 7}: Reimplement the methods @tt{length}, @tt{contains} using
@tt{foldr} to ensure termination for cyclic lists.

@section[#:style 'unnumbered #:tag "lab14:submit"]{Submission}

Submit a zip file of your work at the end of lab.
