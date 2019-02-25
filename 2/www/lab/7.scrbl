#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")
@(require "../utils.rkt")

@lab-title[7]{JSON Equality}

@section[#:style 'unnumbered #:tag "lab7:intro"]{Intro}

Work in ad-hoc pairs.  The two of you will work as a team to solve
problems. At any time, one of you will be the @bold{Head} and the
other will be the @bold{Hands}. The @bold{Head} does the thinking and
the @bold{Hands} does the typing. @bold{Hands} type only what the
@bold{Head} tells them to, but you're free to discuss any issues that
pop up. You should switch off during the lab to make sure each of you
get practice problem solving, dealing with syntax, and getting finger
exercises on the keyboard.

You should start this lab with @link["Lab12.zip"]{this project
skeleton}. Unzip the file into your IdeaProjects directory and open it
with IntelliJ to get started. (The file is called Lab12.zip even
though this is lab 7, but don't worry, you've got the right material.)


@section[#:style 'unnumbered #:tag "lab7:json"]{JSON Reminder}

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

We'll be implementing an @tt{isSame} structural equality method for
the provided JSON classes. The overall structure will be similar to
the @tt{sameShape} method implemented during the last lecture.


@section[#:style 'unnumbered #:tag "lab7:equals"]{Simple Equality}

We want to implement the structural equality for each class that
extends the abstract class @tt{Json}.

Following the outline for double dispatch methods that we saw in
class, add the necessary method signatures and definitions to all of
the JSON classes in order to complete the implementation.

To get you started, we've already done the work to implement isSame
for JInt.

@bold{Ex 1}: Design an @tt{isSameX} methods, one for each of the
possible specific arguments in @tt{JInt}, @tt{JStr}, @tt{Mt},
@tt{Cons}, @tt{MtAssoc}, and @tt{ConsAssoc} (replacing @tt{X} with the
appropriate name).


@bold{Ex 2}: Implement the six @tt{isSame} methods, one in each of
the six JSON classes.


@section[#:style 'unnumbered #:tag "lab7:unordered"]{Beyond Simple Equality}

In real-world JSON, the order of elements in both regular and
association lists does not matter.

@bold{Ex 3}: Implement an order-independent @tt{isSame} method for
@tt{Cons} and @tt{ConsAssoc}.  Design any helper methods you may need.


@section[#:style 'unnumbered #:tag "lab7:submit"]{Submission}

Submit a zip file of your work at the end of lab.
