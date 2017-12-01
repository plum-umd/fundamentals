#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab8"]{Lab 8: Textbox, Message, History}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{Beginning Student
Language}. Require the HtDP2e
@link["https://docs.racket-lang.org/teachpack/2htdpimage.html"]{image} and
@link["https://docs.racket-lang.org/teachpack/2htdpuniverse.html"]{universe}
libraries at the top of your definitions: @racketblock[(require 2htdp/image)
(require 2htdp/universe)]

Make sure you follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Open your solutions to
@link["https://cs.umd.edu/class/fall2017/cmsc131A/lab6.html"]{lab 6} and
@link["https://cs.umd.edu/class/fall2017/cmsc131A/lab7.html"]{lab 7}. If you
haven't already completed labs 6 and 7, do so before starting this lab.

Make sure you save and submit your definitions, we will be extending this
program in future labs.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab8:last"]{Last Time}

In the previous two labs, we have designed data definitions for @tt{Message},
@tt{History}, and @tt{Textbox}, and implemented a number of useful functions:
@tt{message->image}, @tt{add-message}, @tt{textbox->image}, and
@tt{textbox-handle-key}.

We finished lab 7 with a functioning textbox.


@section[#:style 'unnumbered #:tag "lab8:reflect"]{Two Sides of the Cursor}

@larger{@bold{Ex 1}}: In
@link["https://cs.umd.edu/class/fall2017/cmsc131A/assign4.html"]{assignment 4},
you implemented a text editor with a two @tt{String} fields, @tt{pre} and
@tt{post}:

@#reader scribble/comment-reader (racketblock
(define-struct editor (pre post))
;; An Editor is a (make-editor String String)
;; Interp: An Editor represents a text editor, where for some Editor e
;; - (editor-pre  e) is the content before the cursor and
;; - (editor-post e) is the content after the cursor.
)

Recall our data definition for @tt{Textbox}es:

@#reader scribble/comment-reader (racketblock
;; A Natural is a non-negative integer.
(define-struct textbox (content cursor))
;; A Textbox is a (make-textbox String Natural)
;; Interp: a textbox (make-textbox str n) has its cursor 
;; immediately before the `n'th character of `str'. The
;; cursor's value must never exceed the length of the
;; string.
)

Are the two representations, Textbox and Editor, in some sense equivalent?
Justify why or why not in a comment in your definitions window.

@larger{@bold{Ex 2}}: Design the function @tt{editor->textbox} that converts an
@tt{Editor} to a @tt{Textbox}.

@larger{@bold{Ex 3}}: Design the function @tt{textbox->editor} that converts a
@tt{Textbox} to an @tt{Editor}.


@section[#:style 'unnumbered #:tag "lab8:client"]{From @tt{Textbox} to
@tt{ChatClient}}

@larger{@bold{Ex 4}}: Create a data definition ChatClient that includes a
@tt{Textbox}, @tt{History}, and the @tt{String} name of the user sending
messages.

@larger{@bold{Ex 5}}: Create the template @tt{chat-template : ChatClient -> ???}
for all functions that operate on @tt{ChatClient}s.

@larger{@colorize["red"]{@bold{Hint}}}: Remember our mantra for all composite
data: What can we do with it?

@bold{Tear it apart!}


@section[#:style 'unnumbered #:tag "lab8:sendit"]{Send It Already!}

Swap @bold{Head} and @bold{Hands}!

@larger{@bold{Ex 6}}: Design a function @tt{create-client : String ->
ChatClient} that, given the user's name, returns a new @tt{ChatClient} with an
empty @tt{Textbox}, an empty @tt{History}, and that user's name.

@larger{@bold{Ex 7}}: Design a function @tt{draw-client : ChatClient -> Image}
that draws the chat client as a 500x500 pixel window with the @tt{Textbox} on
the bottom with the @tt{History} above.

@larger{@bold{Ex 8}}: Design a function @tt{client-message : ChatClient ->
Message} that, given a @tt{ChatClient}, creates a @tt{Message} using the
contents of the @tt{Textbox} and the client's user as the sender.

@larger{@bold{Ex 9}}: Design a function @tt{send-message : ChatClient ->
ChatClient} that adds the current client @tt{Message} to the @tt{History}. The
@tt{Textbox} should be emptied once the @tt{Message} is sent.

@larger{@bold{Ex 10}}: Design a function @tt{handle-key : ChatClient KeyEvent ->
ChatClient} with all of the functionality of @tt{textbox-handle-key}, but also
with ability to send messages with @tt{send-message} when given the return key
(the KeyEvent @racket["\r"]).

@larger{@bold{Ex 11}}: Using @racket[big-bang] and your @tt{create-client},
@tt{draw-client}, and @tt{handle-key}, send messages to yourself in your
chat client!
