#lang scribble/manual
@(require "../utils.rkt")

@title[#:tag "assign04"]{5/29: Visiting and Equality}

Due: 5/29, midnight by svn.

Language: Java.

You must complete this assignment with your partner and should not
discuss solutions with anyone but your partner and the course staff.

This assignment is due Tuesday at midnight.

@section{Follow-up from Lab}

Complete exercises 5-9 from @seclink["lab04"]{Lab 4} and 1-12 from
@seclink["lab05"]{Lab 5}.

@section{Visitor pattern for Binary Trees}

Develop the visitor pattern for the binary trees data definition you
developed in Lab 4.

Re-develop the methods of exercise 6-8 of Lab 4 as visitors.

@section{Fixing Coffee}

Take a look at the code we wrote showing the perils of @tt{instanceof}
and casts, which is on the @secref{Blog}.  Develop two corrections to
that code---one using safe casting and one using double dispatch---so
that all of the test cases pass.  (Your solutions should be in two
separate files.)

@section{Yahtzee}

There is yet another approach to structural equality that we didn't
discuss in class, but which you have all the concepts needed to carry
out.  We can define equality by way of a visitor that computes whether
the visited value is the same as some given value.

Develop the visitor pattern for the @tt{Drink} data definition and
design an implementation of @tt{DrinkVisitor<Boolean>} that computes
whether the visited drink is structurally equal to a given drink.  You
may find it useful to develop helper visitors.

@section{Same XML}

Revisit your XML assignment and develop a @tt{same} method that
determines whether this XML is the same as some given XML.  Two XML
documents should be considered the same if they are structurally
equal.  You may use any approach you'd like, so long as it does not
involve @tt{instanceof} and casts (``safe casts'' are OK).

@section{Same XML with unorder attributes}

Real XML considers the order of attributes to be irrelevant, so for
example @tt{<img src="mom.jpg" alt="the best">...</img>} is considered
to equivalent to @tt{<img alt="the best" src="mom.jpg">...</img>},
assuming the ``@tt{...}'' are equivalent.

Modify your @tt{same} method so that it ignores the order of
attributes when considering whether two XML documents are the same.

@section{Concrete XML}

Develop a method for XML that produces the string representation of an
XML document, i.e., the @tt{toXMLString} method should produce the
string @tt{"<a href=\"google.com\">Search</a>"} from the
representation of @tt{<a href="google.com">Search</a>}.  Note that
@tt{\"} is how to write a double-quote inside of a string, so @tt{"I
said \"Hello\""} is the string representation of the text @tt{I said
"Hello"}.