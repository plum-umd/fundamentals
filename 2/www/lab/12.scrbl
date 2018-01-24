#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab12"]{Lab 12: Chatting with Text}

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

Open your current @tt{ChatClient} implementation from
@link["https://cs.umd.edu/class/fall2017/cmsc131A/Labs.html"]{labs} 6-8, 10,
11. Make sure you've completed these labs before you continue with this lab and
save/submit your definitions. We will be extending this program in future labs.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab12:new"]{Our New History}

In @secref{lab11} we changed our @emph{History} data definition to a
@emph{ListofMessage} and worked toward handling longer messages. In this lab
we'll get our client up and running with our more interesting @emph{History}.

@larger{@bold{Ex 1}}: Modify all of your example @emph{ChatClient}s to reflect
the new data definition for @emph{History}.

@larger{@bold{Ex 2}}: In a comment in your @italic{definitions window}, write
down a list of all functions that need to change based on our new @emph{History}
(it should include at least the functions @tt{create-client}, @tt{draw-client},
and @tt{add-message}).

@larger{@colorize["red"]{@bold{Hint}}}: Without changing anything else in your
program, run your old tests with your new @emph{ChatClient} examples. Then write
down every function that has failing tests. This is your TODO-list for updating
your current implementation to account for our new data definition.

@larger{@bold{Note}}: If you have no examples or failing tests, you didn't
follow the design recipe up to this point and this exercise is much more
difficult.

@larger{@bold{Ex 3}}: Redesign those functions such that they operate on the new
@emph{History} data definition. For the function @tt{draw-client}, first get the
history drawn without regard to message length.


@section[#:style 'unnumbered #:tag "lab12:words"]{Lines of Words}

Swap @bold{Head} and @bold{Hands}!

@larger{@bold{Ex 5}}: Design a function @tt{words->string} that appends the
@emph{Word}s inside the given @emph{ListofWord}s into a single string with a
space between each @emph{Word}.

@larger{@bold{Ex 6}}: Create a data definition and template for a
@emph{ListofListofWord}s.

@larger{@bold{Ex 7}}: Design a function @tt{lowords->string} that appends the
@emph{ListofWord}s inside the given @emph{ListofListofWord}s into a single
string with newlines between each @emph{ListofWord} and a space between each
@emph{Word}.

@larger{@bold{Ex 8}}: Design a function @tt{lowords->image} that renders the
given list of lists of @emph{Word}s into an image using @tt{text} with newlines
between each @emph{ListofWord} and spaces between each @emph{Word}.

@larger{@bold{Ex 9}}: Using @tt{string->words} from the last lab, design a
function @tt{string->lowords} that, given a @emph{String} @tt{s} and a
@emph{Natural} @tt{n}, returns a @emph{ListofListofWord} where each
@emph{ListofWord} is no more than @tt{n} @emph{Word}s long.

@larger{@bold{Ex 10}}: Using @tt{string->words} from the last lab, design a
function @tt{string->lowords/char} that, given a @emph{String} @tt{s} and a
@emph{Natural} @tt{n}, returns a @emph{ListofListofWord} where each
@emph{ListofWord} is no more than @tt{n} @emph{Char}s long.


@section[#:style 'unnumbered #:tag "lab12:lines"]{Lines of Text}

Swap @bold{Head} and @bold{Hands}!

@larger{@bold{Ex 11}}: Create a data definition and template for a
@emph{ListofImage}s.

@larger{@bold{Ex 12}}: Design a function @tt{string->loimage} that, given a
@emph{String} @tt{s} and a @emph{Natural} @tt{n}, returns a
@emph{ListofImage}s where each @emph{Image} is no more than @tt{n}
pixels wide.

@larger{@bold{Ex 13}}: Use the function @tt{string->loimage} to wrap lines of
@emph{History} correctly in @tt{draw-client}.
