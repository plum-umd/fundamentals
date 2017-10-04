#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab11"]{Lab 11: Chatting with Words}

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
@link["https://cs.umd.edu/class/fall2017/cmsc131A/Labs.html"]{labs} 6-8,
10. Make sure you've completed these labs before you continue with this lab and
save/submit your definitions. We will be extending this program in future labs.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab11:lines"]{Strings in Lines}

Our current @tt{ChatClient} has three parts: the client's @tt{String} name, the
client's @tt{Textbox}, and the client's @tt{Image} chat history. An @tt{Image}
chat history poses a number of difficulties. How can we search for messages with
certain words in them? How should we render the history if the size of the chat
window changes? We can solve these problems and more by changing the
representation of the history to a @tt{ListofMessage}.

@larger{@bold{Ex 1}}: Design the data definition @tt{ListofMessage} that can
hold arbitrarily many @tt{Message}s. Use @racket[cons] and @racket['()] in your
implementation of @tt{ListofMessage}.

@larger{@bold{Ex 2}}: Write down the template @tt{msg-list-template :
ListofMessage -> ???} for functions that operate on @tt{ListofMessage}s.

But we're not done yet. To change the meaning of @tt{History} we have to

@larger{@bold{Ex 3}}: Update the data definition @tt{History} to be defined as
some @tt{ListofMessage}.

But we've got a problem. All the functions we defined that expect or produce
values of type @tt{History} no longer satisfy the specification. They expect or
produce @tt{Image}s. We need to fix that.

As some of you discovered when defining the original @tt{add-message}, there are
a number of interesting edge-cases when rendering @tt{String}s as @tt{Image}s,
one of the trickiest being how to split long content into multiple lines to fit
the window. We'll need to split a @tt{Message} into multiple lines of
@tt{String}s.

@larger{@bold{Ex 4}}: Design the data definition @tt{ListofString} that can hold
arbitrarily many @tt{String}s. We suggest you use @racket[cons] and @racket['()]
in your implementation of @tt{ListofString}; but feel free to roll-your-own
structures if you'd prefer.

@larger{@bold{Ex 5}}: Write down the template @tt{str-list-template :
ListofString -> ???} for functions that operate on @tt{ListofString}s.

@larger{@bold{Ex 6}}: Define the function @tt{string->lines : String Natural ->
ListofString}, that given a @tt{String} and a maximum character width for each
line of text, returns correctly ordered @racket[substring]s of the input as a
@tt{ListofString}s.

For example:
@racketinput[(string->lines "How many lines will this be?" 10)]
@racketresult[(cons "How many l"
                    (cons "ines will "
                          (cons "this be?" empty)))]

The function @tt{string->lines} breaks up strings to arbitrary character
lengths. This has the undesirable feature of splitting some words into multiple
lines, like the word @racket["lines"] in the above example.

Instead, we should represent lines as a series of words.

Turning a string into a list words is trickier than it sounds. We need to
identify whitespace in the string, remove it, and stitch the other characters
together into words.


@section[#:style 'unnumbered #:tag "lab11:words"]{Strings to Words}

Swap @bold{Head} and @bold{Hands}!

@larger{@bold{Note}}: To help us design the function @tt{string->words},
consider the following data definitions that distinguish two kinds of
@tt{Char}s.

@#reader scribble/comment-reader (racketblock
;; A WhiteSpace is a @tt{Char} for which @racket[char-whitespace?] returns @racket[#true].
;; A WordChar is a @tt{Char} for which @racket[char-whitespace?] returns @racket[#false].
)

@larger{@bold{Ex 7}}: Taken together, do the data definitions @tt{WhiteSpace}
and @tt{WordChar} describe the same set of values as @tt{Char}? If so, what's
the point of giving the new data definitions? There are at least two good
reasons.

One of those reasons is that we can use these data definitions to define other
data definitions, such as @tt{Word}s:
@#reader scribble/comment-reader (racketblock
;; A Word is a String where each character is a WordChar.
)

@larger{@bold{Ex 8}}: Calling @racket[string->list] on a @tt{String} returns a
@tt{ListofChar}. Can you name a more specific data definition for values that
@racket[string->list] returns given a @tt{Word}?

Now we can give you the following data definitions for @tt{ListofChar} and
@tt{ListofWord}, and use them to define a function that groups a single
@tt{String} into a @tt{ListofWord}s. The helper function
@tt{add-char-to-first-word} strays a bit from the templates you've seen; we'll
discuss templates for functions over multiple arguments in an upcoming lecture.

For now, copy the following into your @italic{definitions window}.

@#reader scribble/comment-reader (racketblock
;; A ListofChar is one of:
;; - '()
;; - (cons WhiteSpace ListofChar)
;; - (cons WordChar ListofChar)
;;
;; char-list-template : ListofChar -> ???
(define (char-list-template cs)
  (cond [(empty? cs) ...]
        [else (... (first cs)
                   ...
                   (char-list-template (rest cs)))]))

;; A ListofWord is one of:
;; - '()
;; - (cons Word ListofWord)
;;
;; word-list-template : ListofWord -> ???
(define (word-list-template ws)
  (cond [(empty? ws) ...]
        [else (... (first ws)
                   ...
                   (word-list-template (rest ws)))]))

;; add-char-to-first-word : Char ListofWord -> ListofWord
;; Add the given Char to the first Word in the given list, if it is a
;; WordChar. If it is WhiteSpace, add the empty Word to the front of
;; the result.
(define (add-char-to-first-word c ws)
  (cond [(char-whitespace? c) (cons "" ws)]
        [(empty? ws) (cons (string c) empty)]
        [else (cons (string-append (string c) (car ws)) (rest ws))]))

(check-expect (add-char-to-first-word #\tab '()) (cons "" '()))
(check-expect (add-char-to-first-word #\f '()) (cons "f" '()))
(check-expect (add-char-to-first-word #\f (cons "" '())) (cons "f" '()))
(check-expect (add-char-to-first-word #\o (cons "f" '())) (cons "of" '()))
(check-expect (add-char-to-first-word #\space (cons "oof" '()))
              (cons "" (cons "oof" '())))
(check-expect (add-char-to-first-word #\b (cons "" (cons "oof" '())))
              (cons "b" (cons "oof" '())))

;; chars->words : ListofChar -> ListofWord
;; Group the given list of chars into a list of words in the reverse order.
(define (chars->words cs) '()) ; <- stub

(define foo-chars (cons #\f (cons #\o (cons #\o '()))))
(define bar-chars (cons #\b (cons #\a (cons #\r '()))))
(check-expect (chars->words '()) '())
(check-expect (chars->words foo-chars) (cons "foo" '()))
(check-expect (chars->words bar-chars) (cons "bar" '()))
(check-expect (chars->words (append foo-chars (cons #\space empty) bar-chars))
              (cons "foo" (cons "bar" '())))

;; string->words : String -> ListofWord
;; Group the given string into a list of words, delimited by whitespace.
(define (string->words s) '()) ; <- stub

(check-expect (string->words "") '())
(check-expect (string->words " ") (cons "" '()))
(check-expect (string->words "foo") (cons "foo" '()))
(check-expect (string->words "foo bar") (cons "foo" (cons "bar" '())))
(check-expect (string->words "foo  bar") (cons "foo" (cons "" (cons "bar" '()))))
)

We'll do this in a few steps. We gave you the helper function
@tt{add-char-to-first-word}.

@larger{@bold{Ex 10}}: Define the function @tt{chars->words} that, given a
@tt{ListofChar}, returns a @tt{ListofWord} where the @tt{Char}s of the input are
grouped together into @tt{Word}s. Use the helper function
@tt{add-char-to-first-word} in your definition.

@larger{@bold{Ex 11}}: Define the function @tt{string->words} that, given a
@tt{String}, returns a @tt{ListofWord}. You may want to use the helper functions
@tt{chars->words} and @tt{string->list} in your definition.

@larger{@bold{Ex 12}}: The function @tt{string->words} sometimes includes the
empty string in its output. Under what conditions does this happen? Does this
break its signature?

@larger{@bold{Ex 13}}: Design the function @tt{remove-empties} that, given a
@tt{ListofWord}s, returns a @tt{ListofWord}s with no empty strings in it. Modify
your definition of @tt{string->words} so it does not ever include the empty
@tt{String} in its output.


Cool! In the next lab we'll can break our @tt{Message}s into multiple lines of
@tt{Word}s, so we don't garble the content in our @tt{ChatClient}.
