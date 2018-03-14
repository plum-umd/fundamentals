#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")
@(require "../utils.rkt")

@lab-title[12]{JSON Equality}

@section[#:style 'unnumbered #:tag "lab12:intro"]{Intro}

You'll work in this lab with your
@link["https://piazza.com/class/jcspfhewmdn41y?cid=108"]{recently-assigned
lab partners}.

The two of you will work as a team to solve problems. At any time, one
of you will be the @bold{Head} and the other will be the
@bold{Hands}. The @bold{Head} does the thinking and the @bold{Hands}
does the typing. @bold{Hands} type only what the @bold{Head} tells
them to, but you're free to discuss any issues that pop up. You should
switch off during the lab to make sure each of you get practice
problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

You should start this lab with @link["Lab12.zip"]{this project
skeleton}. Unzip the file into your IdeaProjects directory and open it
with IntelliJ to get started.


@section[#:style 'unnumbered #:tag "lab12:json"]{JSON Reminder}

Recall the data definition for JSON from last semester (translated a
bit to Java):

@verbatim|{
/* A Json is one of:
 * - new JInt(Integer)
 * - new JStr(String)
 * - LoJson
 * - Assoc
 * Interp: JSON data
 */

/* A LoJson is one of:
 * - new Mt()
 * - new Cons(Json first, LoJson rest)
 * Interp: A list of Json data
 */

/* An Assoc is one of:
 * - new MtAssoc()
 * - new ConsAssoc(String key, Json value, Assoc rest)
 * Interp: associations between string keys and Json values
 */
}|

We'll be implementing an @tt{equals} method for the provided JSON
classes. The overall structure will be similar to the @tt{sameShape}
method implemented during Monday's lecture.


@section[#:style 'unnumbered #:tag "lab12:equals"]{Simple Equality}

We want to implement the structural equality for each class that
extends the abstract class @tt{Json}.

As shown in lecture, the @tt{equals} method itself should be quite
simple. Each of the implementing classes @tt{JInt}, @tt{JStr},
@tt{Mt}, @tt{Cons}, @tt{MtAssoc}, and @tt{ConsAssoc} should have the
following as their @tt{equals} implementation:

@verbatim|{
public Boolean equals(Json j) {
    return j.isEqual(this);
}
}|

The @tt{isEqual} method in each of those six classes does the real
work.

@bold{Ex 1}: Implement the six base-case @tt{isEqual} methods, one for
each of the possible specific arguments in @tt{JInt}, @tt{JStr},
@tt{Mt}, @tt{Cons}, @tt{MtAssoc}, and @tt{ConsAssoc}.

For example, the @tt{isEqual(JInt ji)} method is provided in the
skeleton.

@verbatim|{
public Boolean isEqual(JInt ji) {
    return false;
}
}|

@bold{Ex 2}: Implement the six @tt{isEqual} methods, one in each of
the six JSON classes.


@section[#:style 'unnumbered #:tag "lab12:unordered"]{Beyond Simple Equality}

In real-world JSON, the order of elements in both regular and
association lists does not matter.

@bold{Ex 3}: Implement an order-independent @tt{isEqual} method for
@tt{Cons} and @tt{ConsAssoc} using the @tt{length}, @tt{exists}, and
@tt{forAll} methods.
