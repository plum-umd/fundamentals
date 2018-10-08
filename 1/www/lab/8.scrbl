#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab8"]{Lab 8: Lists (& Lists (& Lists))}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/beginner.html"]{Beginning Student
Language}.

Make sure you follow
@link["https://cs.umd.edu/class/fall2018/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.


NEW PARTNERS: Starting with this lab, you have a new partner
assignment.  Check Piazza for your new partner.  Introduce yourself,
exchange contact information, and get to work.  You will be working
together in lab and starting with the current assignment (going out
tonight).


Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab8:los"]{A List of Strings}

@larger{@bold{Ex 1}}: Create a recursive data definition @tt{ListOfStrings} that
can hold an arbitrarily many @tt{String}s. You may use a built-in data structure
in BSL or define your own to implement your data definition.

@larger{@bold{Ex 2}}: Define four example @tt{ListOfStrings}s
(@tt{los0}-@tt{los3}), including an empty @tt{ListOfStrings}, a one-element
@tt{ListOfStrings} that contains the @tt{String} @racket["foo"], a two-element
@tt{ListOfStrings} that contains both @racket["Hello"] and @racket["world"],
and another, longer example of your choice.

@colorize["red"]{@larger{@bold{Hint}}}: If you're having trouble creating these
example @tt{ListOfStrings}, ask one of your TAs to look over your data
definition. The data definition should be written as the answer the question
"What is a @tt{ListOfStrings}?"

@larger{@bold{Ex 3}}: Create the template @tt{los-template : ListOfStrings ->
???} for all functions that operate on a @tt{ListOfStrings}.

@larger{@bold{Ex 4}}: How many cases does your template include? In which case
is a recursive call to the template made?


@section[#:style 'unnumbered #:tag "lab8:maps"]{Working with Many Strings}

@larger{@bold{Ex 5}}: Design a function @tt{fake-news} that, given a
@tt{ListOfStrings}, returns a new @tt{ListOfStrings} where each @tt{String} in
the original is replaced with the string @racket["Fake News!"].

@colorize["red"]{@larger{@bold{Hint}}}: @bold{Follow your template!} The
function @tt{fake-news} should look almost identical to a correct
@tt{los-template}. If you have any trouble implementing @tt{fake-news}, talk to
a TA to see if your template is correct.

@racketblock[(check-expect (fake-news los0) (make-empty-los))
             (check-expect (fake-news los1) (make-los "Fake News!" (make-empty-los)))
             (check-expect (fake-news los2)
                           (make-los "Fake News!" (make-los "Fake News!" (make-empty-los))))]

@larger{@bold{Note}}: You will probably need to modify the
@tt{make-*} function calls in above examples based on your implementation of
@tt{ListOfStrings}. The expected values are an empty @tt{ListOfStrings}, a
single-element @tt{ListOfStrings} containing @racket["Fake News!"], and a
two-element @tt{ListOfStrings} containing two @racket["Fake News!"]s.

@larger{@bold{Ex 6}}: If you give @tt{fake-news} a @tt{ListOfStrings} of length
@tt{N}, what will be the length of the resulting @tt{ListOfStrings}?

@larger{@bold{Ex 7}}: Design a function @tt{los-more-exciting} that, given a
@tt{ListOfStrings}, returns a new @tt{ListOfStrings} where each @tt{String} has
had @racket["!"] appended to its end.

@racketblock[(check-expect (more-exciting los0) (make-empty-los))
             (check-expect (more-exciting los1) (make-los "foo!" (make-empty-los)))
             (check-expect (more-exciting los2)
                           (make-los "Hello!" (make-los "world!" (make-empty-los))))]

@larger{@bold{Ex 8}}: Design a function @tt{los-first-letter} that, given a
@tt{ListOfStrings}, returns a new @tt{ListOfStrings} where each @tt{String} has
been replaced with the first letter in that string.

@racketblock[(check-expect (los-first-letter los0) (make-empty-los))
             (check-expect (los-first-letter los1) (make-los "f" (make-empty-los)))
             (check-expect (los-first-letter los2)
                           (make-los "H" (make-los "w" (make-empty-los))))]


@section[#:style 'unnumbered #:tag "lab8:reduce"]{Strings -> Other Things}

Swap @bold{Head} and @bold{Hands}!

@larger{@bold{Ex 9}}: Design a function @tt{los-length} that, given a
@tt{ListOfStrings}, returns the number of @tt{String}s in the list.

@larger{@bold{Ex 10}}: Design a function @tt{los-all-empty?} that returns
@racket[#true] only when all @tt{Strings} in the @tt{ListOfStrings} are the
empty string @racket[""]. Make sure you test the function on inputs that return
@racket[#true] as well as inputs that return @racket[#false].

@larger{@bold{Note}}: The function @tt{los-all-empty?} should still follow your
template (i.e. have the same number of top-level @racket[cond] cases) though it
may use another @racket[cond] expression inside one of those cases.

@colorize["red"]{@larger{@bold{Hint}}}: What should @tt{los-all-empty?} return
for an empty @tt{ListOfStrings}? If you're unsure, try answer these questions:
How many @tt{String}s are there in an empty @tt{ListOfStrings}? Are there any
@tt{String}s that are not the empty @tt{String} inside an empty
@tt{ListOfStrings}?

@larger{@bold{Ex 11}}: Design a function @tt{los-any-empty?} that returns
@racket[#true] if any of the @tt{Strings} in the @tt{ListOfStrings} is the
empty string @racket[""].

@colorize["red"]{@larger{@bold{Hint}}}: What should @tt{los-any-empty?} return
for an empty @tt{ListOfStrings}? If you're unsure, try answer these questions:
How many @tt{String}s are there in an empty @tt{ListOfStrings}? Are there any
empty @tt{String}s inside an empty @tt{ListOfStrings}?

@larger{@bold{Ex 12}}: Design a function @tt{los-all-non-empty?} that returns
@racket[#true] only when all @tt{Strings} in the @tt{ListOfStrings} are
non-empty.

@colorize["red"]{@larger{@bold{Hint}}}: What should @tt{los-all-non-empty?}
return for an empty @tt{ListOfStrings}? If it returns the same answer as
@tt{los-all-empty?}, why? If not, why not?

@larger{@bold{Ex 13}}: Design a function @tt{count-chars} that returns the sum
of the number of characters in all @tt{String}s inside the given
@tt{ListOfStrings}.


@section[#:style 'unnumbered #:tag "lab8:lon"]{Working with Many Numbers}

Swap @bold{Head} and @bold{Hands}!

@larger{@bold{Ex 14}}: Create a recursive data definition @tt{ListOfNats} that
can hold an arbitrarily many @tt{Natural}s (non-negative integers). You may use
a built-in data structure in BSL or define your own to implement your data
definition.

@larger{@bold{Ex 15}}: Define four example @tt{ListOfNats}s
(@tt{lon0}-@tt{lon3}), including an empty @tt{ListOfNats}, a one-element
@tt{ListOfNats} that contains @racket[4], a two-element @tt{ListOfNats} that
contains @racket[0] and @racket[10] and another, longer example of your choice.

@larger{@bold{Ex 16}}: Create the template @tt{lon-template : ListOfNats -> ???}
for all functions that operate on a @tt{ListOfNats}. How does it differ from
@tt{lon-template}. In what ways is it the same?

@larger{@bold{Ex 17}}: Design a function @tt{lon-count} that returns the number
of numbers inside the given @tt{ListOfNats}. How does this function differ from
@tt{los-count}?

@larger{@bold{Ex 18}}: Design a function @tt{lon-sum} that returns the sum of
all numbers inside the given @tt{ListOfNats}.

@colorize["red"]{@larger{@bold{Hint}}}: What should the result be for the sum of
all numbers inside the empty @tt{ListOfNats}?

@larger{@bold{Ex 19}}: Design a function @tt{lon-product} that returns the
product of all numbers inside the given @tt{ListOfNats}.

@colorize["red"]{@larger{@bold{Hint}}}: What should the result be for the
product of all numbers inside the empty @tt{ListOfNats}?

@larger{@bold{Ex 20}}: Design a function @tt{lon-all-odd?} that returns
@racket[#true] only when all numbers in the @tt{ListOfNats} are odd. Make
sure you test the function on inputs that return @racket[#true] as well as
inputs that return @racket[#false].

@larger{@bold{Note}}: The function @tt{lon-all-odd?} should still follow your
template (i.e. have the same number of top-level @racket[cond] cases) though it
may use another @racket[cond] expression inside one of those cases.

@colorize["red"]{@larger{@bold{Hint}}}: What should @tt{lon-all-odd?} return for
an empty @tt{ListOfNats}? How is this related to your answer for
@tt{los-all-empty?}'s base case?

@larger{@bold{Ex 21}}: Design a function @tt{lon-any-even?} that returns
@racket[#true] if any of the numbers inside the given @tt{ListOfNats} is
even.

@larger{@bold{Ex 22}}: Design a function @tt{all-greater-than-n?} that, given a
@tt{ListOfNats} and a @tt{Number} @tt{n}, returns @racket[#true] only if all
numbers inside the @tt{ListOfNats} are greater than the given @tt{n}.


@section[#:style 'unnumbered #:tag "lab8:lon-and-los"]{Strings -> Numbers -> Strings}

Swap @bold{Head} and @bold{Hands}!

@larger{@bold{Ex 21}}: Design a function @tt{strings->counts} that, given a
@tt{ListOfStrings}, returns a @tt{ListOfNats} containing the
@racket[string-length] of each @tt{String} inside the original
@tt{ListOfStrings}.

@larger{@bold{Ex 22}}: Design a function @tt{count-chars/v2} that, given a
@tt{ListOfStrings}, returns the sum of the numbers of characters of each
@tt{String} in the list. Rather than following the template as in the original
@tt{count-chars}, use the functions @tt{strings->counts} and @tt{lon-sum} to
achieve the same goal.

@larger{@bold{Ex 23}}: Design a function @tt{n-many-xs} that, given a
@tt{ListOfNats} and a @tt{String} @tt{x}, returns a @tt{ListOfStrings} where
each number @tt{n} in the given list has been replaced with a @tt{String} with
@tt{n}-many repetitions of the given string @tt{x}.

@colorize["red"]{@larger{@bold{Hint}}}: You may want to use @racket[replicate]
in your solution.

@racketblock[(check-expect (n-many-xs lon0 "131a rocks!") (make-empty-los))
             (check-expect (n-many-xs lon1 "baz") (make-los "bazbazbazbaz" (make-empty-los)))
             (check-expect (n-many-xs lon2 "z")
                           (make-los "" (make-los "zzzzzzzzzz" (make-empty-los))))]

@larger{@bold{Ex 24}}: Design two functions @tt{redact} and @tt{redact/v2} that,
given a @tt{ListOfStrings}, returns a @tt{ListOfStrings} with each string
replaced by a string of the same length, but all characters in the original
@tt{String}s are replaced with @racket["█"]. The first version should follow the
@tt{los-template}; the second version should instead be defined using the
functions @tt{strings->counts} and @tt{n-many-xs}.
