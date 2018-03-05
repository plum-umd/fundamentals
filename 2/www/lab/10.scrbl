#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")
@(require "../utils.rkt")

@lab-title[10]{Exam Practice}

@section[#:style 'unnumbered #:tag "lab10:intro"]{Intro}

You'll work in this lab with your
@link["https://piazza.com/class/jcspfhewmdn41y?cid=27"]{assigned
partner}.

The two of you will work as a team to solve problems. At any time, one
of you will be the @bold{Head} and the other will be the
@bold{Hands}. The @bold{Head} does the thinking and the @bold{Hands}
does the typing. @bold{Hands} type only what the @bold{Head} tells
them to, but you're free to discuss any issues that pop up. You should
switch off during the lab to make sure each of you get practice
problem solving, dealing with syntax, and getting finger exercises on
the keyboard.


@section[#:tag "lab10:init"]{Lab Skeleton}

You must start this lab with @link["Lab10.zip"]{this project
skeleton}. Unzip the file into your IdeaProjects directory and open
it with IntelliJ to get started.

We'll be using the JavaLib image and world library again in this lab;
it's all set up in the project skeleton. See
@link["https://course.ccs.neu.edu/cs2510sp17/image-doc.html"]{the
documentation} for details about available classes/methods.


@section[#:tag "lab10:shapes"]{Clickable Shapes}

A @emph{Shape} is a data structure representing a clickable shape. Its
@tt{X} and @tt{Y} coordinates should be implemented as public
@emph{Integer} fields. All @emph{Shape}s can be drawn as a
@emph{WorldImage} and be clicked (if the position of the click is
inside the boundry of the image).

@bold{Ex 1}: Create an interface @tt{Shape} with two public fields
@tt{Integer x} & @tt{Integer y} and two public methods @tt{WorldImage
draw()} & @tt{Boolean clicked(Posn p)}.

@bold{Ex 2}: Design a class @tt{Circle} that implements the @tt{Shape}
interface. Its constructor should consume three values: @tt{Integer}
coordinates @tt{x} & @tt{y} and an @tt{Integer} radius @tt{r}.

@bold{Ex 3}: Design a class @tt{Rectangle} that implements the
@tt{Shape} interface. Its constructor should consume four values:
@tt{Integer} coordinates @tt{x} & @tt{y} and @tt{Integer} dimensions
@tt{width} & @tt{height}.

@bold{Ex 4}: Design a class @tt{Square} that implements the @tt{Shape}
interface. Its constructor should consume three values: @tt{Integer}
coordinates @tt{x} & @tt{y} and an @tt{Integer} side-length
@tt{side}. @colorize["red"]{Hint}: you can use your @tt{Rectangle}
class in this implementation.


@section[#:tag "lab10:units"]{Convertable Units of Volume}

Swap @bold{Head} and @bold{Hands}.

A unit of @emph{Volume} allows us to easily measure some amount of
liquid. But there are too many units to keep track! Let's design a
program that manipulates units to make the conversion easier. A liter
will be our canonical unit of volume.

@bold{Ex 5}: Implement an interface @tt{Volume} with a single public
method: @tt{Double asLiter()}.

@bold{Ex 6}: Design the trivial class @tt{Liter} that implements
@tt{Volume}. Its constructor should take a single @tt{Double}
argument: the amount of liters.

@bold{Ex 7}: Design the class @tt{Cup} that implements
@tt{Volume}. Its constructor should take a single @tt{Double}
argument: the amount of cups. @bold{Note}: a single US cup is equal to
about 0.24 liters.

@bold{Ex 8}: Design the class @tt{Gallon} that implements
@tt{Volume}. Its constructor should take a single @tt{Double}
argument: the amount of gallons. @bold{Note}: a single US gallon is
equal to about 3.9 liters.

@bold{Ex 9}: Design another class of your choice that implements
@tt{Volume}. Its constructor should take a single @tt{Double}
argument: the amount of whatever unit you're representing.

@bold{Ex 10}: Implement a static method in each of your @tt{Volume}
classes @tt{Volume fromLiter(Double liters)} that given some amount of
liters returns a @tt{Volume} of that class. For example:
@tt{Cup.fromLiter(1).asLiter()} should return a double close to
@tt{1.0}.


@section[#:tag "lab10:dir"]{Directories}

Swap @bold{Head} and @bold{Hands}.

A directory looks a lot like a tree.

A @emph{Directory} is one of:

@itemlist[

  @item{@tt{new Dir(name)}, an empty directory named @tt{name};}

  @item{@tt{new Dir(name, ListOfDir dirs)}, a non-empty directory
  named @tt{name}.}

]

We want to test to see if directories with certain kinds of names
exist.

@bold{Ex 11}: Implement the @tt{Directory} method @tt{Boolean
exists(Predicate<String> p)} that tests whether this directory's name
or some sub-directory's name satisfies the predicate @tt{p}.
