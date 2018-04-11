#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner 2htdp/universe) "helper.rkt")
@(require "../utils.rkt")

@lab-title[17]{Mutable Lists}

@section[#:style 'unnumbered #:tag "lab17:intro"]{Intro}

You'll work in this lab with your
@link["https://piazza.com/class/jcspfhewmdn41y?cid=108"]{lab partners}.

The two of you will work as a team to solve problems. At any time, one
of you will be the @bold{Head} and the other will be the
@bold{Hands}. The @bold{Head} does the thinking and the @bold{Hands}
does the typing. @bold{Hands} type only what the @bold{Head} tells
them to, but you're free to discuss any issues that pop up. You should
switch off during the lab to make sure each of you get practice
problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

You can start this lab with @link["Lab17.zip"]{this project
skeleton}.


@section[#:style 'unnumbered #:tag "lab17:recall"]{Recall}

In @labref{16} we learned how to handle cycles with specialized helper
methods that keep track of the head of a list. Unfortunately, the
normally elegant list operations grew a bit of cruft in the process;
our interface was littered with helper methods that we did not want
to expose.

In this lab, we'll see a similar problem crop up with @emph{mutable
lists}, and we'll learn a new technique that will help hide these
kinds of implementation details.


@section[#:style 'unnumbered #:tag "lab17:contact"]{A List of Contacts}

Your phone's contacts can be modeled as a mutable list of objects with
names and phone numbers. Each contact itself is quite simple:

@verbatim|{
class Contact {
    String name;
    Integer phone;
    Contact(String name, Integer phone) {
        this.name = name;
        this.phone = phone;
    }
}
}|

We've provided you a basic @tt{List} implementation in
@tt{Contacts.java}. Here's a three-contact list that we'll use as our
running example.

@verbatim|{
+----------------------+
| first: Nick 555-1234 |
| rest: ---+           |
+--------- | ----------+
           v
        +----------------------+
        | first: DVH  555-0000 |
        | rest: ---+           |
        +--------- | --------- +
                   v
                +----------------------+
                | first: Sam  555-3141 |
                | rest: ---+           |
                +--------- | --------- +
                           v
                       +-------+
                       +-------+
}|

The new feature we need is the ability to remove old contacts from
this list (as we burn bridges over time), so let's get started!


@section[#:style 'unnumbered #:tag "lab17:remove"]{Removing from Lists}

Removing a contact from the list is a bit tricky. We need to search
through the contact list until we find the @tt{Contact} to remove, but
what do we do then?

@verbatim|{
// In Cons:
public void removeContact(String name) {
    if (this.first.name.equals(name)) {
        // How do we remove the contact?!
    } else {
        this.rest.removeContact(name);
    }
}
}|

Let's look at what we want to happen to our contact list. If we want
to remove our DVH contact, we need the @tt{rest} of the @tt{Cons} cell
with the Nick contact to point to the @tt{Cons} cell Sam contact.

@verbatim|{
+----------------------+
| first: Nick 555-1234 |
| rest:                |
+-- | -----------------+
    |       
    |   +----------------------+
    |   | first: DVH  555-0000 |
    |   | rest: ---+           |
    |   +--------- | --------- +
    |              v
    |           +----------------------+
    +---------->| first: Sam  555-3141 |
                | rest: ---+           |
                +--------- | --------- +
                           v
                       +-------+
                       +-------+
}|

So, when we identify the @tt{Cons} cell that holds the contact we want
to remove, we need to mutate the @emph{previous} @tt{Cons} cell's
@tt{rest} field. We can't do that with @tt{removeContact} as it is
currently written: we need to pass along the previous @tt{Cons} cell
as well.

@bold{Ex 3}: Design the helper method @tt{void removeHelper(Cons prev,
String name)} that removes a contact from the list by @emph{mutating}
the previous @tt{Cons} cell to point to the @tt{rest} of the contact
list. The argument @tt{prev} should always point to the previous
@tt{Cons} cell so its @tt{rest} field can be modified.

@bold{Ex 4}: There is a contact in the list that can't be removed with
this design: which is it?


@section[#:style 'unnumbered #:tag "lab17:sentinel"]{A New Start}

As you noticed in @bold{Ex 4}, it's not possible to remove the first
contact in the list because there is no previous @tt{Cons} cell to
modify.

The solution: create a special cell at the head of the list with no
contact. This special cell is known as a @emph{Sentinel}. It is
similar to a @tt{Cons} cell, in that it has a @tt{rest} field that
points to a @tt{ListofContact}.

@verbatim|{
+-------+
| rest: |
+--- | -+
     v
    +----------------------+
    | first: Nick 555-1234 |
    | rest: ---+           |
    +--------- | ----------+
               v
            +----------------------+
            | first: DVH  555-0000 |
            | rest: ---+           |
            +--------- | --------- +
                       v
                    +----------------------+
                    | first: Sam  555-3141 |
                    | rest: ---+           |
                    +--------- | --------- +
                               v
                           +-------+
                           +-------+
}|

This allows our @tt{removeHelper} to accept either the @tt{Sentinel}
or a @tt{Cons} as the previous cell to mutate. To get keep Java's type
system happy, we need to modify our class hierarchy so @tt{Cons} and
@tt{Sentinel} extend a shared abstract class @tt{Cell}.

@bold{Ex 5}: Add the following classes to your @tt{Contacts.java}:

@verbatim|{
abstract class Cell implements IListofContact {
    IListofContact rest;
    Cell(IListofContact rest) {
        this.rest = rest;
    }
}

class Sentinel extends Cell {
    Sentinel(IListofContact rest) { super(rest); }

    public Optional<Integer> findPhoneNumber(String name) {
        return this.rest.findPhoneNumber(name);
    }

    public void removeContact(String name) {
        this.rest.removeHelper(this, name);
    }

    public void removeHelper(Cell prev, String name) {
        throw new RuntimeException("This will never be called!");
    }
}
}|

@bold{Ex 6}: Modify the @tt{IListofContact.removeHelper} signature to
accept a @tt{Cell} as the previous node to modify. Update your @tt{Mt}
and @tt{Cons} @tt{removeHelper} signatures to follow suit.

@bold{Ex 7}: Change the @tt{Cons} class to extend @tt{Cell}. Remove
the @tt{Cons.rest} field and place a call to @tt{super(rest)} as the
first line in the @tt{Cons} constructor.

At this point, the same tests should pass/fail. But we can modify the
@tt{Main.initData} method to create a list with a @tt{Sentinel} at the
head to get all our tests to pass!

@verbatim|{
void initData() {
    this.contacts =
        new Sentinel(new Cons(nick, new Cons(dvh, new Cons(sam, new Mt()))));
}
}|

@section[#:style 'unnumbered #:tag "lab17:wrapper"]{Wrapping Up}

It's great that our tests pass, but a few issues remain.

First, users of our mutable lists will now need to create instances of
@tt{Sentinel} to wrap around their underlying @tt{Cons} or @tt{Mt}
instances.

Second, the API presented by the interface includes @tt{removeHelper},
which is an implementation detail that should not be used external to
the implementation of the classes.

@bold{Ex 8}: Design a @emph{wrapper class} @tt{MutableList} to
encapsulate the @tt{Sentinel} class. It should accepts either an
@tt{Mt} or @tt{Cons} @tt{IListofContact} in its constructor and have a
single field where it stores that list after placing a @tt{Sentinel}
at its head.

Once you've done this, the @tt{MutableList} class can expose the
proper methods: @tt{removeContact} and @tt{findPhoneNumber} without
exposing the implementation details of the @tt{Sentinel} or
@tt{removeHelper}.

@bold{Ex 9}: Fix up the tests to operate on your new @tt{MutableList}.
