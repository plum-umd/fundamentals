#lang scribble/manual
@(require (for-label (only-in lang/htdp-intermediate-lambda define-struct ... check-expect))
          (for-label (except-in class/0 define-struct ... check-expect))
          (for-label class/universe)
          scribble/bnf
          "../utils.rkt")

@lecture-title[9]{Union, Interfaces, and Lists in Java}

@link["https://umd.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=15905f29-3a7a-4f9e-92a6-a88601577373"]{Video}.

@bold{An IDE for Java}

We're going to be using the IntelliJ integrated development
environment (IDE) to develop and run Java programs.  Like DrRacket, it
will provide useful features to help us in the process of developing
programs.

We'll do a short demo in class and in lab.

@bold{Unions in Java: Traffic Lights}

Let's now turn back to modelling information in Java.  So far we've
seen how to represent compound data such as coordinates.  Another
common kind of data definition is a @emph{union} data definition.
Let's look at a very simple union data definition (that we first
looked at in @racket[class/0]: traffic lights.

We define the union of objects in terms of their interface; the method
signatures that each variant of the union has in common.  In
@racket[class/0] this takes the form:

@class-block{
;; A Light implements
;;
;; Compute the next traffic light after this one
;; next : -> Light
}

In Java, this interface definition looks like this:
@java-block|{
interface Light {
  // Compute the next traffic light after this one
  Light onTick();
}
}|

In @racket[class/0], we would define a class and declare it implements
the interface in a comment.  In Java, we can declare the intention to
implement the interface as part of the class definition:

@java-block|{
class Red implements Light {
  Red() {}
}
}|

@bold{Nullary Constructor by Default.}  Note the @java{Red}
constructor takes no arguments and does nothing, which is what we want
since there are no fields in the @java{Red} class.  As a side note: we
can omit this @emph{nullary} constructor, since it is the default
constructor Java will use if no constructor is defined.

@java-block|{
class Red implements Light {}
}|

Now, this class @emph{says} that it implements the @java{Light}
interface, but so far it has not actually defined the @java{next}
method as required by the @java{Light} interface.

The Java type system will catch this error for us.  If you tried to
compile this program as-is, there would be a compile-time error:

@centered{Error: Red is not abstract and does not override abstract
method next() in Light.}

In the IDE, this static error is often indicated with a red squiggly
underlining of the class definition.

Let's make a stub for @java{next} method:

@java-block|{
class Red implements Light {
  // Compute the next traffic light after this red light
  Light next() {
    return this;
  }
}
}|

@bold{Visibility modifiers.}  As a small wrinkle, we need to place a
@emph{visibility modifier} on this method indicating that the method
can be invoked from outside of the methods in this class:
@java-block|{
class Red implements Light {
  // Compute the next traffic light after this red light
  public Light next() {
    return this;
  }
}
}|

This @java{public} keyword is the visibility modifier.  Using it here
indicates that @java{next} can be called from outside of the
@java{Red} class, which is (implicitly) what's required by the
interface.  The are also visibility modifiers @java{private} and
@java{protected}.  We will mostly ignore visibility issues for now and
assume that methods are public and fields are private.  We will add
modifiers when needed to make programs type-check.  For more details,
see the
@link["https://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html"]{Oracle
tutorial}.

We can define the other variants of @java{Light}, with method stubs:
@java-block|{
class Red implements Light {
  // Compute the next traffic light after this red light
  public Light next() {
    return this;
  }
}

class Yellow implements Light {
  // Compute the next traffic light after this yellow light
  public Light next() {
    return this;
  }
}

class Green implements Light {
  // Compute the next traffic light after this green light
  public Light next() {
    return this;
  }
}
}|

Thinking through some examples, it's easy to come up with the correct
code for the method definitions:

@java-block|{
class Red implements Light {
  // Compute the next traffic light after this red light
  public Light next() {
    return new Green();
  }
}

class Yellow implements Light {
  // Compute the next traffic light after this yellow light
  public Light next() {
    return new Red();
  }
}

class Green implements Light {
  // Compute the next traffic light after this green light
  public Light next() {
    return new Yellow();
  }
}
}|

So now we've seen our first union data definition in Java.

@bold{Grammar: Java with Interfaces}

The grammar of Java programs has grown some now.  In particular a
program now consists of some number of class @emph{or interface}
definitions.  Since a set of objects may be defined as either a class
or interface, we use @nonterm{type-name} to range over things defined
as classes or interfaces.  The grammar is also updated to have
optional visibility modifiers on method definitions.  Here are the
revised parts of the grammar (only):

@(let ([open @litchar|{{}|]
       [close @litchar|{}}|])
   @BNF[(list @nonterm{program}
              @kleenestar[@nonterm{class-or-intf-defn}])
        (list @nonterm{class-or-intf-defn}
              @nonterm{class-defn}
              @nonterm{intf-defn})
        (list @nonterm{intf-defn}
              @BNF-seq[@litchar{interface} @nonterm{int-name} open @nonterm{method-sigs} close])
        (list @nonterm{field-decl}
              @BNF-seq[@nonterm{type-name} @nonterm{field-name} @litchar{;}])
        (list @nonterm{method-sigs}
              @BNF-seq[]
              @BNF-seq[@nonterm{method-sig} @litchar{;} @nonterm{method-sigs}])
        (list @nonterm{method-sig}
              @BNF-seq[@nonterm{type-name} @nonterm{meth-name} @litchar{(} @nonterm{param-list} @litchar{)}])
        (list @nonterm{method-defn}
              @BNF-seq[@optional{@nonterm{vis-modifier}} @nonterm{method-sig} open
                         @litchar{return} @nonterm{expr} @litchar{;} 
                       close])
        (list @nonterm{vis-modifier} @litchar{public} @litchar{private})
        (list @nonterm{type-name} @nonterm{class-name} @nonterm{intf-name})
        (list @nonterm{intf-name} @nonterm{id})])

@bold{Recursive Unions in Java: Lists}

TBD