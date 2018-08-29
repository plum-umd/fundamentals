#lang scribble/manual
@(provide readings)
@(require scribble/core (for-label lang/htdp-beginner))

@title[#:style 'unnumbered #:tag "assign8"]{Assignment 8: Trees, forests, and ML}

@bold{Due:} Monday, October 30, 11:59:59 PM EST.

The following should be completed in cooperation with your latest
assigned partner, @bold{which were newly assigned on Oct 23}. (Partner
assignments are listed on
@link["https://piazza.com/class/j474gwnsd3619n?cid=294"]{Piazza}.)
You may not share code for this assignment with anyone but your
partner.

You must use the design recipe and @secref{style} guidelines to
receive full credit.

@section[#:tag "assign8:prep"]{Preparation}

@(define readings
  @elem{all of
    @link["https://htdp.org/2018-01-06/Book/part_three.html"]{Part III} 
    and through section 19.2 of 
    @link["https://htdp.org/2018-01-06/Book/part_four.html"]{Part IV}})

Make sure you have read @readings of HtDP2e.

@section[#:tag "assign8:trees"]{A walk in the forest}

Edit a file named @bold{EXACTLY} @tt{ft.rkt} for this part of the
assignment. Add your information to the standard file header at the
top of the file.

Complete exercises 310--315 from
@link["https://htdp.org/2018-01-06/Book/part_four.html"]{HtDP2e
Part IV}.  (Use the function name @tt{forest-average-age} in exercise 315 to
avoid conflicting with @tt{average-age} in 311.)



@section[#:tag "assign8:ml"]{Mark it}

Edit a file named @bold{EXACTLY} @tt{ml.rkt} for this part of the
assignment. Add your information to the standard file header at the
top of the file.

Markup languages @emph{abound}.  Examples include HTML, XML, XHTML,
LaTeX, troff, YAML, and many many more.  A @bold{markup language} is
used to annotate textual information with "editor marks" that annotate
how the text should be presented.  For example, a snippet of HTML, the
hypertext markup language for documents on the web, looks like this:

@verbatim{
<h1>Mark it</h1>

<p>
Edit a file named <b>EXACTLY</b> <tt>ml.rkt</tt> for this part of the
assignment. Add your information to the standard file header at the
top of the file.
</p>

<p>
Markup languages <i>abound</i>.  Examples include HTML, XML, XHTML,
LaTeX, troff, YAML, and many many more.  A <b>markup language</b> is
used to...
</p>
}

The content of this document contains @emph{elements}, which are
enclosed in balanced brackets like @tt{<p>} and @tt{</p>}.  Each
element has a @emph{tag}, in this case the tag is "p", and a content,
which may consist of textual data (strings) or elements.

This suggest the following data definitions for a simple, but general
purpose, markup language:

@#reader scribble/comment-reader (racketblock
;; A Content is a [Listof Item]
;; Interp: a collection of content in a markup document

;; An Item is one of:
;; - a String
;; - an Element
;; Interp: an item in a document, either text or an element

;; An Element is a (make-elem Tag Content)
;; Interp: an element is content annotated with a tag
(define-struct elem (tag content))

;; A Tag is a String
;; Interp: the name of the tag, e.g. "p"
)

@bold{Exercise 1} Make an example representing the content in the HTML
document snippet above.

@bold{Exercise 2} Design a function @tt{content-to-HTML} that converts
content to a string representation of the HTML markup.  For example,
convert the example from exercise 1 should produce a string that
starts
@racket["<h1>Mark it</h1><p>Edit the file named <b>EXACTLY</b> <tt>ml.rkt</tt> for..."].

(There are some subtle issues like what to do if there is an item in
the document which is a string that looks like HTML, e.g. "<tt>", but
we will ignore those issues for now.)

@bold{Exercise 3} Design a function @tt{content-contains-tag?} that
determines if an element with a given tag occurs within any part of
the content of document.

@bold{Exercise 4} Design a function @tt{content-length} that sums up
the length of all the textual items with a document.

@bold{Exercise 5} Design a function @tt{content-contains-elem?} that consumes
a predicate on elements and returns true if the content contains any
element that satisfies the predicate.

@bold{Exercise 6} Design a function @tt{content-retag} that takes two
tags and renames every occurrence of the first tag to the second tag
within the given content.

@bold{Exercise 7} Design a function @tt{content-max-nesting} that
determines the maximum level of element nesting within the given
content.  (The max nesting level in the example above is 2.)

@bold{Exercise 8} Design a function @tt{content-text} that produces a
list of all the text that occurs in a document, in the order it
occurs.

@bold{Exercise 9} Design a function @tt{content-skeleton} that, given
content, produces a document that is like the given content, but with
all text removed.

@section[#:tag "assign8:submission"]{Project submission}

You should submit two files named @bold{EXACTLY}: @tt{ft.rkt} and
@tt{ml.rkt}.

Submit your files directly to the submit server by uploading them.
Select each of these files individually using the ``Browse'' button.
Once you have selected all the files, press the ``Submit project!''
button. You do not need to put them in a zip file.
