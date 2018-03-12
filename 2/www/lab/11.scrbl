#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")
@(require "../utils.rkt")

@lab-title[11]{Dispatching Enemies}

@section[#:style 'unnumbered #:tag "lab11:intro"]{Intro}

You'll work in this lab with your
@link["https://piazza.com/class/jcspfhewmdn41y?cid=108"]{@bold{newly-assigned
lab partners}}.

The two of you will work as a team to solve problems. At any time, one
of you will be the @bold{Head} and the other will be the
@bold{Hands}. The @bold{Head} does the thinking and the @bold{Hands}
does the typing. @bold{Hands} type only what the @bold{Head} tells
them to, but you're free to discuss any issues that pop up. You should
switch off during the lab to make sure each of you get practice
problem solving, dealing with syntax, and getting finger exercises on
the keyboard.


@section[#:style 'unnumbered #:tag "lab11:disp"]{Method Dispatch}

Which method gets called when we pass messages to objects? This
section gives a few examples which may not act how you believe they
should. Discuss what you expect should happen with your new partner
before running the code to find out.

If you've never read @emph{Romeo and Juliet}, let's just say that the
Montagues and the Capulets don't get along very well.

We'll represent the Montagues as an empty class.

@verbatim|{
class Montague {}
}|

Romeo is of House Montague. As such, he extends the class @tt{Montague}.

@verbatim|{
class Romeo extends Montague {}
}|

Now the Capulets are known for starting trouble and tend to be rude
when greeting Montagues. Note that if a Capulet sees Romeo, they'll at
least be rude to him by name.

@verbatim|{
class Capulet {
    String name;
    Capulet(String nm) { this.name = nm; }

    String greet(Montague m) {
        return name + ": I bite my thumb at you!";
    }

    String greet(Romeo m) {
        return name + ": I bite my thumb at you, Romeo!";
    }
}
}|

@bold{Ex 1}: If a @tt{Capulet} @tt{greet}s a @tt{Montague}, what will
be said? Write down your expectation, then test this yourself with a
@tt{checkExpect}.

@verbatim|{
Capulet sampson = new Capulet("Sampson");
Montague abram = new Montague();
Romeo romeo = new Romeo();

sampson.greet(abram) => ???
sampson.greet(romeo) => ???
}|


@section[#:style 'unnumbered #:tag "lab11:sing"]{Single Dispatch}

Juliet, of House Capulet, sees Romeo in a different light.

@verbatim|{
class Juliet extends Capulet {
    Juliet() { super("Juliet"); }

    String greet(Romeo m) {
        return name + ": Wherefore art thou, Romeo?";
    }
}
}|

The class @tt{Juliet} extends Capulet, and so shares the default,
thumb-biting virtiol for any @tt{Montague} in general. However,
@tt{Juliet} seems to @tt{greet}s @tt{Romeo} in particular in loving,
longing way. But does this happen when we run the code?

@bold{Ex 2}: What happens when a @tt{Juliet} @tt{greet}s a @tt{Montague} in general?

@verbatim|{
Juliet juliet = new Juliet();
juliet.greet(abram) => ???
}|

@bold{Ex 3}: What happens when a @tt{Juliet} @tt{greet}s @tt{Romeo} in particular?

@verbatim|{
Juliet juliet = new Juliet();
juliet.greet(romeo) => ???
}|


@section[#:style 'unnumbered #:tag "lab11:rose"]{A Rose by Another Name}

Will everything continue so nicely for Romeo & Juliet? Let's see what
happens when these ill-fated lovers put on some hooded cloaks. Or, in
Java terms, let's assign them to variables with the types of their
super classes.

@verbatim|{
Capulet hoodedJuliet = juliet;
Montague hoodedRomeo = romeo;
}|

@bold{Ex 4}: What happens when our hooded @tt{Juliet} @tt{greet}s a
@tt{Montague} in general? Should this be the same or different than the prior
greeting?

@verbatim|{
hoodedJuliet.greet(abram) => ???
}|

@bold{Ex 5}: What happens when our hooded @tt{Juliet} @tt{greet}s the
unhooded @tt{Romeo}? Should this be the same or different than the
prior greeting?

@verbatim|{
hoodedJuliet.greet(romeo) => ???
}|

@bold{Ex 6}: What happens when our hooded @tt{Juliet} @tt{greet}s
our hooded @tt{Romeo}? Should this be the same or different than
the prior greeting? Doth it smell just as sweet?

@verbatim|{
hoodedJuliet.greet(hoodedRomeo) => ???
}|

Let's test one more case to shed some light on the issue.

@bold{Ex 7}: What happens when our unhooded @tt{Juliet} @tt{greet}s
our hooded @tt{Romeo}? Should this be the same or different than
the prior greeting?

@verbatim|{
juliet.greet(hoodedRomeo) => ???
}|

@section[#:style 'unnumbered #:tag "lab11:sing"]{Single Dispatch}

Oh no! Both the hooded and unhooded @tt{Juliet}s bit her thumb at the
hooded Romeo! That's not good. But why did it happen?

When Juliet only sees a hooded cloak with the Montague crest, she
@tt{greet}s the @tt{hoodedRomeo} as she would greet any @tt{Montague}:
with @emph{fightin' words}!

The problem is that Java doesn't know statically that @tt{hoodedRomeo}
is of type @tt{Romeo}, it sees it as any generic @tt{Montague}. But if
that's the case, why does the hooded Juliet know how to correctly
greet the unhooded Romeo?

Think about it this way: Juliet knows who she is regardless of whether
she's wearing a hood or not. So for both @tt{juliet} and
@tt{hoodedJuliet}, the @tt{Juliet} class handles greeting @tt{romeo}
in a loving manner. Java knows @tt{romeo} to be of type @tt{Romeo},
and the @tt{Juliet} class dispatches the correct @tt{greet} method
dynamically.

How can we take advantage of the dynamic dispatch inherent to
@tt{this} to make sure Romeo & Juliet always greet each other kindly?


@section[#:style 'unnumbered #:tag "lab11:doub"]{Double Dispatch}

One solution is to dispatch twice: once inside each class.

Remember that the @tt{Juliet} class knows to @tt{greet} @tt{Romeo}
kindly. Let's make sure that @tt{Romeo} will always @tt{beGreet}ed
properly.

@bold{Ex 8}: Add the following method to @emph{both} the @tt{Montague}
and @tt{Romeo} classes.

@verbatim|{
String beGreeted(Capulet c){
    return c.greet(this);
}
}|

Note that the method only calls the @tt{greet} method inside the
@tt{Capulet} class. But importantly, inside both the @tt{Montague} and
@tt{Romeo} classes the variable @tt{this} knows exactly whether it is
@tt{Romeo} or not, regardless of the location where @tt{beGreeted} was
called.

@bold{Ex 9}: What happens when @tt{romeo} will @tt{beGreeted}ed by
a general @tt{Capulet}?

@verbatim|{
romeo.beGreeted(sampson) => ???
}|

@bold{Ex 10}: What happens when @tt{romeo} will @tt{beGreeted}ed by a
@tt{juliet} in particular?

@verbatim|{
romeo.beGreeted(juliet) => ???
}|

@bold{Ex 11}: What happens when @tt{romeo} will @tt{beGreeted}ed by a
@tt{hoodedJuliet} in particular?

@verbatim|{
romeo.beGreeted(hoodedJuliet) => ???
}|

@bold{Ex 12}: What happens when our @tt{hoodedRomeo} will
@tt{beGreeted}ed by @tt{juliet}?

@verbatim|{
hoodedRomeo.beGreeted(juliet) => ???
}|

@bold{Ex 13}: What happens when our @tt{hoodedRomeo} will
@tt{beGreeted}ed by our @tt{hoodedJuliet}?

@verbatim|{
hoodedRomeo.beGreeted(hoodedJuliet) => ???
}|

