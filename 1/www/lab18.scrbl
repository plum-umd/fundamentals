#lang scribble/manual
@(require scribble/core (for-label lang/htdp-intermediate) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab18"]{Lab 18: Once More with Meaning}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/intermediate-lam.html"]{Intermediate
Student Language with Lambda}.

Make sure you follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab18:technicians"]{Just Some Other Nonsense}

In @secref{lab17} you implemented operations on @emph{Shuffle}s, @emph{Duffle}s,
@emph{Muzzle}s, and @emph{Muff}s with no understanding of the meaning of those
data definitions. Thankfully, the structural templates of operations on those
data are effective regardless of the meaning of that data.

It turns out that @emph{Shuffle}s are equivalent to a data format known as
@link["https://en.wikipedia.org/wiki/JSON"]{JSON}.

@bold{Ex 1}: Don't take our word for it: write in a comment two examples each of
@emph{Shuffle}s, @emph{Duffle}s, @emph{Muzzle}s, and @emph{Muff}s and their
respective representations as @emph{JSON} values.

@#reader scribble/comment-reader (racketblock
;; An Atom is one of:
;; - Number
;; - String
;; Interp: Atomic JSON data.
(define ATOM0 "")
(define ATOM1 42)
(define ATOM2 "brightly, brightly, and with beauty")
;;
(define-struct empty-asc ())
(define-struct asc-pair (key value rest))
;; An Asc is one of:
;; - (make-empty-asc)
;; - (make-asc-pair String JSON Asc)
;; Interp: Ascs represent associations between string keys and JSON values.
(define ASC0 (make-empty-asc))
(define ASC1 (make-asc-pair "foo" ATOM0 ASC0))
(define ASC2 (make-asc-pair "bar" ATOM1 ASC1))
(define ASC3 (make-asc-pair "baz" ASC1 ASC2))
;;
;; A JSON is one of:
;; - Atom
;; - Asc
;; - [Listof JSON]
;; Interp: A subset of the JavaScript Object Notation specification.
(define JSON0 '())
(define JSON1 (list ATOM0 ASC0))
(define JSON2 (list ASC1 ATOM2 ATOM1))
)

The structure of the data remains unchanged. The templates for operations over
@emph{JSON} are as follows:

@#reader scribble/comment-reader (racketblock
;; atom? : JSON -> Boolean
(define (atom? j) (or (string? j) (number? j)))

;; asc? : JSON -> Boolean
(define (asc? j) (or (empty-asc? j) (asc-pair? j)))

;; json-template : JSON -> ???
;; The template for all operations over JSON.
(define (json-template j)
  (cond [(atom? j) (atom-template j)]
        [(asc? j) (asc-template j)]
        [else (... (json-template (first j))
                   ...
                   (json-template (rest j))
                   ...)]))

;; atom-template : Atom -> ???
;; The template for all operations over Atoms.
(define (atom-template a)
  (cond [(string? a) (... a ...)]
        [(number? a) (... a ...)]))

;; asc-template : Asc -> ???
;; The template for all operations over Ascs.
(define (asc-template m)
  (cond [(empty-asc? m) ...]
        [(asc-pair? m) (... (asc-pair-key m)
                            ...
                            (json-template (asc-pair-value m))
                            ...
                            (asc-template (asc-pair-rest m))
                            ...)]))
)

Remember our mantras for these templates: @bold{search} through
@emph{itemizations} to find the proper case; @bold{destroy} @emph{composites} by
tearing them apart. Each time we make progress (by finding the proper case or by
pulling out smaller pieces of data) we hand the value over to its respective
template.

As with @emph{Shuffle}s, we can easily turn @emph{JSON} values into strings by
following their templates. But the code below has a few bugs...

@bold{Ex 2}: Copy @tt{intersperse} from @secref{lab17} and the functions
@tt{{json,atom,asc}->string} (below) into your @emph{defintions window}. Fix
the bugs in the code and any other mistakes (including those in signatures).
Make all tests pass.

@#reader scribble/comment-reader (racketblock
;; atom->string : Atom -> String
;; Convert the given atomic JSON value to a string.
(define (atom->string a)
  (cond [(string? a) (append "\"" a "\"")]))
(check-expect (atom->string ATOM0) "\"\"")
(check-expect (atom->string ATOM1) "42")
(check-expect (atom->string ATOM2) "\"brightly, brightly, and with beauty\"")

;; asc->string : Asc -> Number
;; Convert the given JSON asc to a string.
(define (asc->string m)
  (local [;; asc->strings : Asc -> [Listof String]
          ;; Convert each pair inside the asc to a string.
          (define (asc->strings m)
            (cond [(empty-asc? m) '()]
                  [else (cons (pair->string (asc-pair-key m) (asc-pair-key m))
                              (asc->strings (asc-pair-rest m)))]))
          ;; pair->string : String JSON -> String
          ;; Create a string representation of a key-value pair.
          (define (pair->string k v)
            (string-append "\"" k "\":" (json->string v)))]
    (intersperse (asc->strings m) ", " "{" "}")))
(check-expect (asc->string ASC0) "{}")
(check-expect (asc->string ASC1) "{\"foo\":\"\"}")
(check-expect (asc->string ASC2) "{\"bar\":42, \"foo\":\"\"}")
(check-expect (asc->string ASC3)
              "{\"baz\":{\"foo\":\"\"}, \"bar\":42, \"foo\":\"\"}")

;; json->string : JSON -> String
;; Convert the given JSON value into its string representation.
(define (json->string j)
  (cond [(atom? j) (atom->string j)]
        [(asc? j) (atom->string j)]
        [else (intersperse (map json->string j) ", " "[" "]")]))
(check-expect (json->string JSON0) "[]")
(check-expect (json->string JSON1)
              "[\"\", {}]")
(check-expect (json->string JSON2)
              "[{\"foo\":\"\"}, \"brightly, brightly, and with beauty\", 42]")
)

@section[#:style 'unnumbered #:tag "lab18:alist"]{Working with @emph{Asc}s}

Swap @bold{Head} and @bold{Hands}!

In the last lab, we designed a function @tt{muzzle-find} such that if the
@emph{Muzzle} contains a @emph{Muzzle} street with @tt{key} in the @tt{oobleck}
field, it returns the @emph{Duffle} in the @tt{tumble} field (and
@racket[#false] otherwise).

@bold{Ex 3}: Design the function @tt{asc-find} that, given a @emph{Asc}
and a @emph{String} @tt{key}, returns the first @emph{JSON} value associated
with @tt{key} or @racket[#false] if no such key exists.

@bold{Note}: To write a proper signature for @tt{asc-find}, we need an
itemization of either @emph{JSON} values or @racket[#false]. We need a data
definition to represent the union of those values.

@bold{Ex 4}: Design a data definition @emph{EitherFalseOrJSON} for values that
are either @emph{JSON} or @racket[#false]. Remember, to @emph{design} a data
definition you must give a template as well.


There are many functions like @tt{asc-find} that we wish to write that may
return one of two otherwise unassociated data types. Rather than littering our
definitions with simple unions like @emph{EitherFalseOrJSON}, we can

@bold{Ex 5}: Design a data definition @emph{[Either X Y]} that generalizes over
any two-case itemizations. Put some thought into how the right-hand side of the
two @racket[cond] clauses should work, you may need to accept more than one
argument in the @tt{either-template}.

@bold{Ex 6}: Design the function @tt{asc-remove} that, given a
@emph{Asc} and a @emph{String} @tt{key}, returns a new @emph{Asc} with
all key/value assocciations with keys @racket[string=?] to @tt{key} removed.


@section[#:style 'unnumbered #:tag "lab18:jmap"]{Mapping @emph{Atom}s in
@emph{JSON}}

Swap @bold{Head} and @bold{Hands}!

@bold{Ex 7}: Design the function @tt{json-add1} that given a @emph{JSON} value,
returns a new @emph{JSON} value where each number has been incremented by 1 and
each string has had @racket["+1"] appended to its end.

@colorize["red"]{Hint}: Per the template, you'll need to design functions
@tt{atom-add1} and @tt{asc-add1} to properly implement @tt{json-add1}.

@#reader scribble/comment-reader (racketblock
(check-expect (json-add1 JSON2) (list (make-asc-pair "foo" "+1" ASC0)
                                      "brightly, brightly, and with beauty+1"
                                      43))
(check-expect (json-add1 ATOM0) "+1")
(check-expect (json-add1 ATOM1) 43)
(check-expect (json-add1 ASC1) (make-asc-pair "foo" "+1" ASC0))
(check-expect (json-add1 ASC3)
              (make-asc-pair "baz" (make-asc-pair "foo" "+1" ASC0)
                        (make-asc-pair "bar" 43
                                       (make-asc-pair "foo" "+1" ASC0))))
)

@bold{Ex 8}: Design the function @tt{json-no-numbers} that given a @emph{JSON}
value, returns a new @emph{JSON} value where each number has been converted to a
string.

@colorize["red"]{Hint}: Per the template, you'll need to design functions
@tt{atom-no-numbers} and @tt{asc-no-numbers} to properly implement
@tt{json-no-numbers}.

@#reader scribble/comment-reader (racketblock
(check-expect (json-no-numbers JSON2) (list ASC1 ATOM2 "42"))
(check-expect (json-no-numbers ATOM0) "")
(check-expect (json-no-numbers ATOM1) "42")
(check-expect (json-no-numbers ASC1) ASC1)
(check-expect (json-no-numbers ASC3)
              (make-asc-pair "baz" ASC1 (make-asc-pair "bar" "42" ASC1)))
)

@bold{Ex 9}: Design the following three functions, which map operations over the
atomic values inside @emph{JSON} values.

@#reader scribble/comment-reader (racketblock
;; json-atom-map : JSON (Number -> JSON) (String -> JSON) -> JSON
;; Map @tt{nf} and @tt{sf} over the atomic values in the given JSON value.
(define (json-atom-map j nf sf) j) ;; <- stub

;; atom-map : Atom (Number -> JSON) (String -> JSON) -> JSON
;; Map @tt{nf} and @tt{sf} over the given atomic value.
(define (atom-map a nf sf) a) ;; <- stub

;; asc-atom-map : Asc (Number -> JSON) (String -> JSON) -> Asc
;; Map @tt{nf} and @tt{sf} over the atomic values inside @tt{m}'s values.
(define (atom-map m nf sf) m) ;; <- stub
)

@bold{Ex 10}: Define the functions @tt{json-add1/2} and @tt{json-no-numbers/2} in
terms of the @emph{Atom} mapping functions.


@section[#:style 'unnumbered #:tag "lab18:jfilter"]{Filtering @emph{Asc}s}

@bold{Ex 11}: Design a function @tt{foos-are-awful} that, given a @emph{JSON}
value, returns a new @emph{JSON} with any @emph{Asc} key/value pair with the
key @racket["foo"] removed.

@#reader scribble/comment-reader (racketblock
(check-expect (foos-are-awful JSON1) JSON1)
(check-expect (foos-are-awful JSON2) (list ASC0 ATOM2 ATOM1))
(check-expect (foos-are-awful ASC2) (make-asc-pair "bar" ATOM1 ASC0))
(check-expect (foos-are-awful ASC3) 
              (make-asc-pair "baz" ASC0 (make-asc-pair "bar" ATOM1 ASC0)))
)

@bold{Ex 12}: Design a function @tt{no-more-numbers} that, given a
@emph{JSON} value, returns a new @emph{JSON} with any @emph{Asc} key/value
pairs with numeric values removed.

@#reader scribble/comment-reader (racketblock
(check-expect (no-more-numbers JSON1) JSON1)
(check-expect (no-more-numbers JSON2) JSON2)
(check-expect (no-more-numbers ASC2) ASC1)
(check-expect (no-more-numbers ASC3) (make-asc-pair "baz" ASC1 ASC1))
)

@bold{Ex 13}: Design a function @tt{asc-filter-out} that, given an @emph{Asc}
and a function @tt{remove? : String JSON -> Boolean}, returns a new @emph{JSON}
with any @emph{Asc} key/value pairs that satisfy @tt{remove?} removed.

@colorize["red"]{Hint}: Per the template, you'll need a function
@tt{json-filter-out} to properly design @tt{asc-filter-out}.

@bold{Ex 14}: Define the functions @tt{foos-are-awful/2} and
@tt{no-more-numbers/2} in terms of @tt{asc-filter-out}.
