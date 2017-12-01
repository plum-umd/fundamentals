#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab13"]{Lab 13: Chatting with Chip}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/beginner-abbr.html"]{BSL+}. Require
the HtDP2e
@link["https://docs.racket-lang.org/teachpack/2htdpimage.html"]{image} and
@link["https://docs.racket-lang.org/teachpack/2htdpuniverse.html"]{universe}
libraries at the top of your definitions: @racketblock[(require 2htdp/image)
(require 2htdp/universe)]

Make sure you follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Open your current @tt{ChatClient} implementation from
@link["https://cs.umd.edu/class/fall2017/cmsc131A/Labs.html"]{labs} 6-8,
10-12. Make sure you've completed these labs before you continue with this lab
and save/submit your definitions. We will be extending this program in future
labs.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab13:solipsism"]{What's the point?}

In @secref{lab12} we updated our @emph{ChatClient} implementation to work on our
new @emph{History} data definition. You should have a fully working client,
including properly wrapped lines of text in messages that appear in the history.

But why have a chat client without someone to chat with?

A single @emph{ChatClient} isn't enough to have a conversation. Let's extend the
state of the world to include another @emph{ChatClient}.

@larger{@bold{Ex 1}}: Design a data definition @emph{ChatRoom} to represent a
conversation between two @emph{ChatClient}s, with fields referred to as @tt{me}
and @tt{other}.

@bold{Note}: The history of the two @emph{ChatClient}s is likely to be
identical--don't worry about that redundancy for this lab.

@bold{Note}: If you define structures or functions that implement the
@emph{ChatRoom} data definition, be sure to follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} for
their identifiers. More importantly, don't use @emph{ChatRoom} in their names
(thought @tt{chat-room} or the more brief @tt{chat} are fine choices).

Remember, there is no BSL identifier @emph{ChatRoom} and it has no meaning at
run-time. Data definitions are specifications--rules we agree to when
interacting with certain types of data. We agree that functions that return
@emph{ChatRoom}s return values of the specified form. We agree to only give
functions that expect @emph{ChatRoom}s values of the specified form.


@section[#:style 'unnumbered #:tag "lab13:other"]{Chatting with Someone}

A @emph{ChatRoom} has two parts, of course we can go ahead and...

@larger{@bold{Ex 2}} Write down the template @tt{chat-template : ChatRoom ->
???} for functions that operate on @emph{ChatRoom}s.

@larger{@bold{Ex 3}} Define two example @emph{ChatClient}s: @tt{CLIENT-ME} and
@tt{CLIENT-CHIP}.

Our previous @emph{WorldState} in our @racket[big-bang] invocation was
@emph{ChatClient}. We need to define and use functions that create and
manipulate our new @emph{WorldState}: @emph{ChatRoom}.

@larger{@bold{Ex 4}}: In a comment in your @italic{definitions window}, write
down a list of all functions that need to be written for @emph{ChatRoom}s.

@colorize["red"]{@bold{Hint}}: Just as when we implemented
@emph{TwoChip}s, don't modify your old code that operates on @emph{ChatClient}s
since you should be able to reuse much of it.

@bold{Note}: Draw the @emph{ChatRoom} as two @emph{ChatClient}s
side-by-side.

Swap @bold{Head} and @bold{Hands}!

@larger{@bold{Ex 5}}: Get the @emph{ChatRoom} with two clients up and running so
that they can have a conversation.


@section[#:style 'unnumbered #:tag "lab13:others"]{You Should Have Seen This
Coming...}

Swap @bold{Head} and @bold{Hands}!

@larger{@bold{Ex 6}}: Extend the definition of @emph{ChatRoom} to handle an
arbitrary number of clients.

@larger{@bold{Ex 7}}: Get the @emph{ChatRoom} with an arbitrary number of
clients up and running so that they can all have a conversation.
