#lang scribble/manual
@(require scribble/eval
          racket/sandbox
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ... check-expect))
          (for-label (except-in class/0 define-struct ... check-expect))
          (for-label class/universe)
          "../utils.rkt")

@lecture-title[8]{Introducing Java: Syntax and Semantics}

@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=41c2986c-d55e-4548-b43f-a8820148000b"]{Video}.

You've now seen several programming languages: BSL, ISL, ISL+, and
@tt{class/0}.  Now time for one more: Java.  Let's start by looking at
the @emph{syntax}, the way Java programs are written.

Here's a comment in @tt{class/0}:
@class-block{
;; Hi there
}

Here's a comment in Java:
@java-block{
// Hi there
}

Here's a block comment in @tt{class/0}:
@class-block{
#| Hi
   there |#
}

Java:
@class-block{
/* Hi
   there */
}

A class definition in @tt{class/0}:
@class-block{
;; A Coord is a (new coord% Integer Integer)
(define-class coord%
  (fields x y))
}

A class definition in Java:
@java-block|{
class Coord {
  Integer x;
  Integer y;
}
}|

Notice here that the convention for class names in Java is to
capitalize them.  Also notice that, because Java is a typed language,
it requires us to specify the type of the fields.  In this case, the
fields @tt{x} and @tt{y} are both objects belonging to the
@tt{Integer} class.  So in Java, some of the information that we're
used to writing down as part of data definitions becomes part of the
actual program text; not just a comment.

The above class definition defines a new class of values, called
@java{Coord}s.  A @java{Coord} value is an object with an @java{x} and
@java{y} field, both of which hold @java{Integer} objects.

One thing that Java requires us to write explicitly is a
@emph{constructor}, whereas @racket[class/0] made the constructors for
us.  The way constructors work in @racket[class/0] is to say
@racket[new], a class name, and then give the appropriate number of
expressions (one for each field); the value of these expressions are
used to populate the fields of the object.  We can write such a
constructor in Java by revising the class definition:

@java-block|{
class Coord {
  Integer x;
  Integer y;

  Coord(Integer x, Integer y) {
    this.x = x;
    this.y = y;
  }
}
}|

This constructor definition says if you call the constructor (we will
see how in a moment) with two integers, it will populate the fields of
a @java{Coord} object with the given integers.

To make a @tt{Coord} in @racket[class/0], you write:
@class-block{
(new coord% 3 4)
}

To make a @java{Coord} in Java, you write:
@java-block{
new Coord(3, 4)
}

In general, the arguments of the constructor can be arbitrary
expressions that produce integers, e.g.
@java-block{
new Coord(2 + 1, 2 * 2)
}

In @racket[class/0], if you have a @tt{coord%} object @racket[o] and
want to extract the value in the @tt{x} field, you write:
@class-block{
(send o x)
}

In Java, the notation for @racket[send] is a ``dot'' written between the
object expression and the field name:

@java-block{
o.x
}

So for example:
@java-block{
new Coord(2 + 1, 2 * 2).x
}
would produce @java{3} when run.

To add some functionality to @tt{Coord} objects in @racket[class/0],
we'd write:
@class-block{
;; A Coord is a (new coord% Integer Integer)
(define-class coord%
  (fields x y)

  ;; Integer Integer -> Coord
  (define (move dx dy)
    (new coord% (+ (send this x) dx) (+ (send this y) dy))))
}

To write the same thing in Java:
@java-block|{
class Coord {
  Integer x;
  Integer y;

  Coord(Integer x, Integer y) {
    this.x = x;
    this.y = y;
  }

  Coord move(Integer dx, Integer dy) {
    return new Coord(this.x + dx, this.y + dy);
  }
}
}|

There are a few things to note here.  First, all of the peices from
the @racket[class/0] definition are present, but the signature that
existed in comments is now part of the code.  The part of the
signature to the right of the @tt{->} is now to the @emph{left} of the
method name in Java.  The other thing to note is the use of the
@java{return} keyword and @java{;} around the expression in the body
of the method; this is signalling that the method produces whatever
the expression evaluates to.

Invoking the method uses the send notation similar to accessing a
field:

@java-block|{
new Coord(3, 4).move(1, 2)
}|

The above expression will produce @java{new Coord(4, 6)} when run.

To actually run programs, we will have to either run the programs from
within a programming environment, like DrRacket but for Java, or use a
Java @emph{compiler} and run the Java interpreter from the command
line.  These will be covered in demos in class.

