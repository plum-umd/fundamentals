#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner 2htdp/universe) "helper.rkt")
@(require "../utils.rkt")

@lab-title[17]{Stacks of Stuff}

@section[#:style 'unnumbered #:tag "lab17:intro"]{Intro}

You'll work in this lab with your ad-hoc partner.

The two of you will work as a team to solve problems. At any time, one
of you will be the @bold{Head} and the other will be the
@bold{Hands}. The @bold{Head} does the thinking and the @bold{Hands}
does the typing. @bold{Hands} type only what the @bold{Head} tells
them to, but you're free to discuss any issues that pop up. You should
switch off during the lab to make sure each of you get practice
problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

You can start this lab with @link["Lab19.zip"]{this project
skeleton}.


@section[#:style 'unnumbered #:tag "lab17:simple"]{A Simple Stack}

A @emph{stack} is a simple data structure much like a list. We can
@tt{push} new elements on to the top of the stack and we can @tt{pop}
the top element off of the stack (if it exists). Note that these
operations mutate the underlying data structure, so once we've popped
an element off the stack, it remains off of the stack.

@verbatim|{
IStackof<String> stack = new Stackof<>();
stack.height();  // => 0
stack.push("foo");
stack.height();  // => 1
stack.push("bar");
stack.height();  // => 2
stack.pop();     // => Optional.of("bar")
stack.pop();     // => Optional.of("foo")
stack.pop();     // => Optional.empty()
stack.height();  // => 0
}|

The file @tt{Stackof.java} has a skeleton of a stack
implementation. It also contains a simple @tt{Listof} implementation
which @emph{should not need to be modified}.

@bold{Ex 1}: Implement the core, effectful operations @tt{push} and
@tt{pop} for @tt{Stack}. @bold{Note}: some of the tests that confirm
the height of the stack will fail until you finish exercises 2 and 3,
but the tests that call @tt{get} and @tt{isPresent} should pass.

@bold{Ex 2}: Implement @tt{Stackof.foldr}. You should delegate this
operation to the underlying list.

@bold{Ex 3}: Implement @tt{Stackof.height} using
@tt{Stackof.foldr}. All the stack tests should pass.

@section[#:style 'unnumbered #:tag "lab17:complex"]{Stack with Constraints}

We can add constraints about what kinds of elements may be pushed on
top of the stack. Recall the interface
@link["https://docs.oracle.com/javase/9/docs/api/java/lang/Comparable.html"]{Comparable<T>},
which allows data to compare itself to data of the same kind.

Integers implement comparable as @tt{<}:

@verbatim|{
1.compareTo(2)    // => -1
10.compareTo(-1)  // =>  1
42.compareTo(42)  // =>  0
}|

We can make an extension of the stack data structure that only allows
elements smaller than the top element (i.e. where the new element
compares to the top element as -1). For integers that means smaller
values must be pushed on to larger values.

@bold{Ex 4}: Implement @tt{class OrdStackof<X extends Comparable<X>>
extends Stackof<X>}. You should only need to override the method
@tt{push} from the original stack. It should only allow values smaller
than the top to be pushed on top of the stack, and simply not push any
other elements.

@bold{Ex 5}: Implement any non-trivial class that implements
@tt{Comparable} (e.g. a person with a name in alphabetical
order). Write a test similar to @tt{testOrdPushPop} to confirm that
the order is respected by @tt{OrdStackof}.

@section[#:style 'unnumbered #:tag "lab15:submit"]{Submission}

Submit a zip file of your work at the end of lab.
