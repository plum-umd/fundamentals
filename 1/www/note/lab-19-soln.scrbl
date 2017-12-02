#lang scribble/manual
@(require racket/sandbox
          scribble/example)

@title{Solving lab 19: From Functions to Signatures}

Lab 19 asks you to provide the most general signature for functions like the following:

@#reader scribble/comment-reader (racketblock
; brillig : 
(define (brillig x)
  (= 1 (modulo x 2)))
)

The way to approach problems like this is to play @emph{code
detective}.  Start by collect evidence of what you know, then use that
to make deductions about other things you know.  Keep doing this until
you can't deduce anything more, and you've arrived at the most general
signature.

Let's walk through an example.  What do you know about @tt{brillig}?
It's a function that takes one input.  Let's make a signature
involving variables to capture what we know so far; as we continue,
these variables may get solved and go away.  So far we have:

@#reader scribble/comment-reader (racketblock
; brillig : A -> B
)

We know that much just from look at:

@#reader scribble/comment-reader (racketblock
(define (brillig x) ...)
)

Now, let's look at the body of the function to try and learn more
information about @tt{A} and @tt{B}.

Q: How is the parameter @tt{x} used?  A: it is the first argument to
@racket[modulo].  Follow-up Q: what is the signature of
@racket[modulo]? A: @tt{Number Number -> Number}.  So we can conclude
@tt{A} = @tt{Number}.

Q: What kind of value does the body of the function produce?  A: It
produces whatever kind of value @racket[=] produces.  Follow-up Q:
What does @racket[=] produce? A: A @tt{Boolean}.  So we can conclude
@tt{B} = @tt{Boolean}.

Since we've solved for the two unknowns, @tt{A}, @tt{B}, we are done
and can now give the most general signature:

@#reader scribble/comment-reader (racketblock
; brillig : Number -> Boolean
)

Using this signature, go back to the code and confirm it makes sense.

Let's do another.

@#reader scribble/comment-reader (racketblock
; slithy :
(define (slithy m n)
  (> (length m) n))
)

We can see from the form of the definition that @tt{slithy} is a
function taking two arguments.  Assigning variables for the unknowns,
we get:

@#reader scribble/comment-reader (racketblock
; slithy : A B -> C
)

How are the arguments used?  

Well, @tt{m} is the argument of @tt{length} and @tt{length} expects a
list of some kind.  What kind?  We don't know yet, so assign a new
variable for this unknown, say @tt{D}.  Now we know @tt{A} =
@tt{[Listof D]}.

Switching to @tt{n}, it is the second argument of @tt{>}, so @tt{B} = @tt{Number}.

What kind of value does the body of the function produce?  Whatever
kind of value @tt{>} produces, i.e. @tt{Boolean}s.

So far we have:
@#reader scribble/comment-reader (racketblock
; slithy : [Listof D] Number -> Boolean
)

Are there any facts we haven't accounted for?  No, so we're done, but
there are still unkowns: @tt{D}.  Take another look at the code.  Does
the code impose any constraints on what kind of elements are in the
list?  Does it treat them like numbers, string, functions, etc.?  No,
it doesn't do anything with the elements of the list, so there's
nothing more to learn about @tt{D} and it remains as a parameter in
the most general signature:

@#reader scribble/comment-reader (racketblock
; slithy : [D] [Listof D] Number -> Boolean
)

Go back and make sure this signature makes sense for the function.

Another:

@#reader scribble/comment-reader (racketblock
; outgrabe :
(define (outgrabe x y)
  (map x (append y y)))
)

From the definition, we get:

@#reader scribble/comment-reader (racketblock
; outgrabe : A B -> C
)

How is the first parameter, @tt{x}, used?  It is the first argument of
@racket[map].  The first argument of @racket[map] must be a function
that takes one argument, so we know @tt{A} must be @tt{D -> E} where
@tt{D} and @tt{E} are new unknowns.

How is the second parameter, @tt{y}, used?  It is both the first and
second argument of @tt{append}, which takes two lists and produces a
list of the same kind of element.  So @tt{B} must be a list of some
kind @tt{[Listof F]}.

Now, since the result of @tt{(append y y)} is the second argument to
@racket[map] and we know the elements of the list and the input of the
function must match, we can conclude @tt{F} = @tt{D}.

What about @tt{C}?  What kind of value does the body of the function
produce?  It produces whatever kind of value @tt{map} produces, which
is a list of elements produced by the given function.  Hence, @tt{C} =
@tt{Listof E}.

Collecting what we know, we have:
@#reader scribble/comment-reader (racketblock
; outgrabe : [D -> E] [Listof D] -> [Listof E]
)

Are there any facts we haven't accounted for or anything more we can
say about @tt{D} or @tt{E}?  No, so we're done and @tt{D} and @tt{E}
are parameters to the signature:

@#reader scribble/comment-reader (racketblock
; outgrabe : [D E] [D -> E] [Listof D] -> [Listof E]
)

Go back and make sure this signature makes sense for the function.

NOTE: It's important to note that whenever we determine that one
variable is equal to another, we then only use one or the other, but
not both.  For example, we know @tt{D} = @tt{F}, but it would be a
mistake to give this function the signature:

@#reader scribble/comment-reader (racketblock
; WRONG-outgrabe : [D E F] [D -> E] [Listof F] -> [Listof E]
)

The problem is having two separate parameters, which sould really be a
single one, makes it seem like @tt{D} and @tt{E} can be instantiated
separately to different things.  So each set of equal variables,
should be represnted by a single variable in the final signature.



Moving on: 

@#reader scribble/comment-reader (racketblock
; uffish :
(define (uffish x)
  (cond [(number? x) (+ x 10)]
        [(string? x) (string-length x)]))
)

We know:

@#reader scribble/comment-reader (racketblock
; uffish : A -> B
)

What is done with @tt{x}?  The function asks if it's a number or a
string (and nothing else), so @tt{A} is either a number or a string.
We could make a data definition for such things:
@#reader scribble/comment-reader (racketblock
;; A NumOrString is one of
;; - A Number
;; - A string
)

What kind of value does the function produce?  It produces whatever
the @racket[cond] produces, which in term produces whatever @tt{+}
produces @emph{and} whatever @tt{string-length} produces,
i.e. @tt{Number}s.

So:
@#reader scribble/comment-reader (racketblock
; uffish : NumOrString -> Number
)

Next:
@#reader scribble/comment-reader (racketblock
; frabjous :
(define (frabjous a b c)
  (a (b c)))
)

We know:
@#reader scribble/comment-reader (racketblock
; frabjous : A B C -> D
)

What do we know about @tt{A} based on how @tt{a} is used?  It is a
function of one argument.  So @tt{A} = @tt{E -> F} for new unknowns.

@#reader scribble/comment-reader (racketblock
; frabjous : [E -> F] B C -> D
)


What do we know about @tt{B} based on how @tt{b} is used? It is a
function of one argument.  So @tt{B} = @tt{G -> H} for new unknowns.

@#reader scribble/comment-reader (racketblock
; frabjous : [E -> F] [G -> H] C -> D
)

What do we know about @tt{C} based on how @tt{c} is used?  It is the
argument of @tt{b}.  So @tt{G} = @tt{C}.

@#reader scribble/comment-reader (racketblock
; frabjous : [E -> F] [C -> H] C -> D
)

Are there facts we haven't accounted for?  Yes, the result of @tt{(b
c)} is the argument of @tt{a}, so the input of @tt{a} and the result
of @tt{b} must match, i.e. @tt{E} = @tt{H}.

@#reader scribble/comment-reader (racketblock
; frabjous : [E -> F] [C -> E] C -> D
)


What does the function produce?  Whatever @tt{a} produces, so @tt{D} =
@tt{F}.

@#reader scribble/comment-reader (racketblock
; frabjous : [E -> D] [C -> E] C -> D
)

There's nothing more we can deduce, so the remaining unknowns are
parameters:

@#reader scribble/comment-reader (racketblock
; frabjous : [C D E] [E -> D] [C -> E] C -> D
)

Next, picking up the pace:

@#reader scribble/comment-reader (racketblock
; callooh :
(define (callooh a b)
  (a 10 (or (b 0) (b 1))))

; callooh : A B -> C

; because (b 0) and (b 1)

; callooh : A [Numbder -> D] -> C

; because (or (b 0) (b 1))

; callooh : A [Numbder -> Boolean] -> C

; because (a 10 (or ...))

; callooh : [Number Boolean -> G] [Numbder -> Boolean] -> C

; because (define (callooh ...) (a ...))

; callooh : [Number Boolean -> C] [Numbder -> Boolean] -> C

; done:

; callooh : [C] [Number Boolean -> C] [Numbder -> Boolean] -> C
)

Last:

@#reader scribble/comment-reader (racketblock
; callay :
(define (callay q)
  (frabjous (λ (d) (+ d 42)) q "day"))

; callay : A -> B
)

Now since we apply a function with a parametric signature, we need to
use the parameters of @tt{frabjous} (first making them distinct from
the unknowns we have so far, which they already are) and try to solve
for them:

@#reader scribble/comment-reader (racketblock
frabjous : [E -> D] [C -> E] C -> D
)

Since @racket["day"] is the third argument, @tt{C} = @tt{String}.

@#reader scribble/comment-reader (racketblock
frabjous : [E -> D] [String -> E] String -> D
)

Since @racket[q] is the second argument, @tt{A} = @tt{[String -> E]}.

Since @racket[(λ (d) (+ d 42))] is the first argument, and it has the
signature @tt{[Number -> Number]}, we know @tt{E} = @tt{Number} and
@tt{D} = @tt{Number}.

@#reader scribble/comment-reader (racketblock
frabjous : [Number -> Number] [String -> Number] String -> Number
)

Coming back to @tt{callay}, we know @tt{A} = @tt{[String -> E]} and
@tt{E} = @tt{Number}, so @tt{A} = @tt{[String -> Number]}:

@#reader scribble/comment-reader (racketblock
; callay : [String -> Number] -> B
)

What does the function produce?  Whatever @tt{frabjous} produces.  So
@tt{B} = @tt{Number}:

@#reader scribble/comment-reader (racketblock
; callay : [String -> Number] -> Number
)

And we're done.
