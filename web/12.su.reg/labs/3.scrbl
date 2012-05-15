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

@title[#:tag "lab03"]{5/15: Subversion, Redux; Moar Recursive Methods with XML}

The goal of this lab is to introduce more aspects of our work
environment: the Subversion revision control system and the Tester
library. In the second part of the lab, you will practice writing some
recursive data definitions and their methods.

@lab:section{Getting started with Subversion}

Let's try this again...

Subversion is a revision control system.  It helps you keep track of
the changes to a document over the course of its lifetime.  It can be
used for any kind of document: papers, notes, spreadsheets, web pages,
etc., but it is often used for the source code of programs.  We're
going to be using Subversion (svn) for the remainder of the course in
order to track changes to code written for labs and homeworks and we
will use it as the mechanism for turing in and receiving feedback on
assignments.

@exercise{Skim the course notes on @secref{Subversion}.}

Subversion is available on all the lab machines, but you may also want
to install a Subversion client (there are several to choose from) on
your personal computer.

The course repository, @tt{cs2510summer2012}, has been created with a
directory for each pair and a directory for each student.  You will
only have access to access to your user directory and your pair
directory.  No one but the course staff has access to your user
directory and no one but your partner and the course staff has access
to your pair directory.

@exercise{Check out a copy of your user directory in the
@tt{cs2510summer2012} repository.  Verify that your solution to
assignment 1 is in it.}

You should use this directory to store lab material, notes, or
anything else you'd like that shouldn't be shared with others (beside
the staff).  We will use this directory to communicate (non-pair)
grades back to you.

Remember that no changes are made to the repository until you
successfully commit.  Until then, any changes you make are only stored
on the computer you're working on.  Therefore you should commit early
and often (like voting).

@exercise{Check out a copy of your pair directory in the
@tt{cs2510summer2012} repository.}

You should use this directory to store assignments.  We will use this
directory to communicate pair grades back to you and your partner.

@exercise{Within your user directory, create two directories called
@tt{lab1} and @tt{lab2}.  Add your Java source code to for each lab to
these directories.  Check these directories in to the repository and
commit.}


@lab:section{XML}

XML, the ``Extensible Markup Language'', is a ubiquitous format for
exchanging data on the interwebs.  It is also used extensively in
modern office-productively software produced by Microsoft, Apple, and
the open-source community.  XML is fairly simple: it consists of
content that is ``marked up'' with tags.

Here are some simple examples of XML:

@verbatim{
I am XML!
}

The above is just plain content with no tags.  We can tag certain
parts of the content with a pair of open and close tags to delimit the
tagged region:

@verbatim{
I am <yell>XML</yell>!
}

Here the content @tt{XML} is tagged with the @tt{yell} tag.  The tags can
nest, as in:

@verbatim{
I am <yell><italic>X</italic>ML</yell>!
}

Here the content @tt{XML} is again tagged with @tt{yell}, but the
@tt{X} is also tagged with the @tt{italic} tag.

Tags can also carry attributes that associate data with a tag.  For
example, we may want to yell at a certain volume:

@verbatim{
I am <yell volume="30db"><italic>X</italic>ML</yell>!
}

Here the @tt{yell} tag carries a @tt{volume} attribute with the value
@tt{30db}.  Moreover, you can add an arbitrary number of attributes to
a tag, so we can specifiy both the volume and the duration of a yell:

@verbatim{
I am <yell volume="30db" duration="5sec"><italic>X</italic>ML</yell>!
}

If we step back and think about how to represent XML, we arrive at the
following sketch:

XML is either just plain content (no tags), or it is tagged XML,
i.e. it is some tag, some list of attributes and values, and some XML.

This leads to a pretty straightforward (recursive) data definition.
Unfortunately, it's not quite adequate for the examples we've written.
In particular, we want to following, which is valid XML, to be
representable:

@verbatim{
<first>Bake a cake.</first> <then>Eat <yell>it!</yell></then>
}

Here we see that XML can really be an arbitrarily long sequence of
plain content and tagged content and that within a tag, there is also
a sequence.  To revise our sketch we have:

XML is a sequence of XML fragments.  An XML fragment is either plain
content (no tags), or tagged XML, i.e. some tag, some attributes and
values, and some XML.

@exercise{Design class definitions for representing XML.  Translate
all of the above examples into your representation.}

Now that you've done the hard part of designing the data for
representing XML, you can write programs to operate on XML.  If your
data definition is good, these should be easy to write.  If it's not,
well... they will probably be more challenging.

Let's start with some simple ones:

@exercise{Design a @tt{contentLength} method computes the length of
the content in an XML document.  The tags and attributes should not
contribute to the length.}

@exercise{Design a @tt{maxNest} method that computes the maximum
number of tag nesting in a document.}

@exercise{Design a @tt{hasTag} method that determines if an XML
document contains a given tag.}

@exercise{Design a @tt{hasAttribute} method that determines if an XML
document contains a given attribute.}

Now for some slightly more involved methods:

@exercise{Design a @tt{hasAttributeInTag} method that determines if an
XML document contains a given attribute within a given tag.}

@exercise{Design a @tt{mute} method replaces the value of any
@tt{volume} attribute with a value of @tt{0db}.}

@exercise{Design a @tt{muteYell} method replaces the value of any
@tt{volume} attribute within a @tt{yell} tag with a value of
@tt{0db}.}

@exercise{Design a @tt{renderAsString} method that converts XML to
plain strings by removing all tags and attributes.}

@exercise{Design a @tt{updateAttribute} method that is given an
attribute name and value and updates all occurrence of that attribute
to the given value.}

@exercise{Design a @tt{renameTag} method that changes all occurrences
of a given tag to another given tag.}

Be sure to commit your work to svn before you leave.