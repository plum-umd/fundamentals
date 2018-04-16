#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner 2htdp/universe) "helper.rkt")
@(require "../utils.rkt")

@lab-title[18]{Social Graphs}

@section[#:style 'unnumbered #:tag "lab18:intro"]{Intro}

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

You can start this lab with @link["Lab18.zip"]{this project
skeleton}.


@section[#:style 'unnumbered #:tag "lab18:recall"]{Recall}

In @labref{16} we learned how to handle cycles in simple lists.

In this lab, we'll see cycles appear in a social network. To handle
these cycles, we'll practice writing accumulating operations over a
graph of buddies.


@section[#:style 'unnumbered #:tag "lab18:people"]{A Person with Buddies}

This lab focuses on a small group of friends named Alice, Bob, Carol,
Donald, and Ester. Each person has a name a few people they consider
their buddies. The class @tt{Person} models a person with buddies.

We asked each of these five people to tell us their buddies. The
results of that survey are shown here:

@verbatim|{
              +--------+
          --->| Carol  |<-------
         /    +--------+------  \
        /         ^ |         \  \
       /          | v          v  \
+--------+    +--------+<---+--------+
|  Bob   |--->| Alice  |    | Donald |
+--------+    +--------+    +--------+
                  |
                  v
              +--------+
              | Ester  |--
              +--------+  \
                      ^   /
                       \_/
}|

The arrow from Alice to Ester means that Alice said Ester was her
buddy. Note: there is no arrow from Ester to Alice, so Ester does not
include Alice in her list of buddies.

@bold{Ex 1}: In the file @tt{Lab18.java}, implement @tt{Main.initData}
such that the five friends are initialized with the buddies given in
the above graph.


@section[#:style 'unnumbered #:tag "lab18:dops"]{Direct Buddies}

We call the people listed in some @tt{Person}'s buddies the
@emph{direct buddies}. The buddies of your buddies are @emph{indirect
buddies}.

@bold{Ex 2}: Some people technically qualify as both direct and
indirect buddies. Find an example of such a pair in the social graph
above.

@bold{Ex 3}: Design the method @tt{Person.hasDirectBuddy(Person
that)}, which returns true if the given person is a @emph{direct
buddy} of this person. Write at least two tests for each of our
example people: one that returns true and one that returns false.

@bold{Ex 4}: Design the method @tt{Person.countCommonBuddies(Person
that)}, which returns the number of direct buddies shared by @tt{this}
and @tt{that} person. Write at least one test for each of our example
people.


@section[#:style 'unnumbered #:tag "lab18:eops"]{Extended Buddies}

If we're throwing a party, we need to invite our buddies, their
buddies, and their buddies' buddies, etc. We call the set of all
direct and indirect buddies the @emph{extended buddies} of a person.

@bold{Ex 5}: Design the method @tt{Person.hasExtendedBuddy(Person
that)}, which returns true only if the given person is one of this
person's buddies or buddies' extended buddies. Write at least one test
returning true and one test returning false for each example person.

@bold{Ex 6}: Design the method @tt{Person.partyCount()}, which returns
the number of extended buddies that would be invited to a party thrown
by this person. Note: the party technically includes this person, so
make sure not to leave them out when calculating the @tt{partyCount}!
Write a test for each of our example people.
