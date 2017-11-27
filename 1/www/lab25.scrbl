#lang scribble/manual
@(require scribble/core scribble/examples "helper.rkt"
          (for-label lang/htdp-intermediate-lambda))

@title[#:style 'unnumbered #:tag "lab25"]{Lab 25: Generating Trees}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/intermediate-lam.html"]{Intermediate
Student Language with Lambda}.

Make sure you follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab25:review"]{Last Time}

In the last lab (@secref{lab24}) the first exercise had you build a deck of
cards from a list of suits and a list of ranks. Each of the four suits were
combined with each of the thirteen ranks for a total of fifty-two cards.

@bold{Ex 1}: Generalize your solution to build the deck by designing the
function @tt{cart-prod}, which given two lists will build a list of two-element
lists representing the
@link["https://en.wikipedia.org/wiki/Cartesian_product"]{Cartesian product} of
the input. This must be implemented with explicit recursion (no higher-order
list functions allowed).

These tests show you the intended functionality. The order of your results may
vary, but not the order of the paired elements.

@#reader scribble/comment-reader (racketblock
(check-expect (cart-prod '(1 2) '()) '())
(check-expect (cart-prod '() '(1 2)) '())
(check-expect (cart-prod '(1 2 3) '(a b))
              '((3 b) (3 a) (2 b) (2 a) (1 b) (1 a)))
(check-expect (cart-prod '(♠ ♥ ♦ ♣) '(2 3 4))
              '((♣ 4) (♣ 3) (♣ 2)
                (♦ 4) (♦ 3) (♦ 2)
                (♥ 4) (♥ 3) (♥ 2)
                (♠ 4) (♠ 3) (♠ 2)))
)

@; ;; cart-prod : [Listof X] [Listof Y] -> [Listof (list X Y)]
@; ;; Create all trees with each of ls and rs on the left and right.
@; (define (cart-prod ls rs)
@;   (foldl (λ (l a) (foldl (λ (r a) (cons (list l r) a)) a rs)) '() ls))

@bold{Ex 2}: Which type of 2-List template does @tt{cart-prod} exemplify? Does
it iterate over only one list, iterate over one and then the other list, or
iterate over both lists in simultaneously?

@bold{Ex 3}: Re-implement @tt{cart-prod} using higher-order functions.

@section[#:style 'unnumbered #:tag "lab25:leafy"]{Trees with Leaves}

It's easy to represent binary trees using lists.

@#reader scribble/comment-reader (racketblock
;; A @emph{Tree} is one of:
;; - 'leaf
;; - (list @emph{Tree} @emph{Tree})
)

The symbol @racket['leaf] is a @emph{Tree} of height 0.

@bold{Ex 3}: Write down all of the trees of heights 0, 1, and 2. There should be
1, 1, and 3 such trees, resp.

@bold{Ex 4}: Describe in a comment (and to your partner) how the trees of height
2 are created using the trees of heights 1 and 0. What does it actually mean to
be a tree of height 2? What must be true of the subtrees?

@bold{Ex 5}: Design the function @tt{trees=} which given a @emph{Natural} @tt{h}
generates a list of all @emph{Tree}s of height @tt{h}. Use insights gleaned from
@bold{Ex 4} to generate the correct recursive calls.

@colorize["red"]{@bold{Hint}}: You may want to also design a function
@tt{trees<}, which returns a list of all @emph{Tree}s of height less than the
given @emph{Natural} @tt{h}.

@bold{Another @colorize["red"]{Hint}}: You may find the function @tt{cart-prod}
useful in your design of @tt{trees=}.

@colorize["red"]{@bold{WARNING}}: There are 457653 trees of height 5. You'll
likely run out of memory enumerating the trees of height 6.

@; ;; trees= : Natural -> [Listof Tree]
@; ;; Generate all trees of height H.
@; ;; Key insight: a tree of height H must have a tree of
@; ;;   height H-1 on the left or right.
@; (define (trees= h)
@;   (cond [(zero? h) '(leaf)]
@;         [else (let ([h-2+ (trees< (- h 1))]
@;                     [h-1  (trees= (- h 1))])
@;                 (append (cart-prod h-2+ h-1)
@;                         (cart-prod h-1 h-2+)
@;                         (cart-prod h-1 h-1)))]))

@; (check-expect (trees= 0) '(leaf))
@; (check-expect (trees= 1) '((leaf leaf)))
@; (check-satisfied (trees= 2)
@;                  (let ([answers '((leaf (leaf leaf)) ((leaf leaf) leaf)
@;                                   ((leaf leaf) (leaf leaf)))])
@;                    (λ (ts) (andmap (λ (tr) (member? tr answers)) ts))))
@; (check-expect (length (trees= 3)) 21)
@; (check-expect (length (trees= 4)) 651)
@; (check-expect (length (trees= 5)) 457653)

@; ;; trees< : Natural -> [Listof Tree]
@; ;; Generate all trees of height less than H.
@; (define (trees< h)
@;   (cond [(zero? h) '()]
@;         [else (append (trees= (- h 1)) (trees< (- h 1)))]))

@; (check-expect (trees< 0) '())
@; (check-expect (trees< 1) '(leaf))
@; (check-satisfied (trees< 2)
@;                  (λ (ts) (and (member? '(leaf leaf) ts)
@;                               (member? 'leaf ts))))

@bold{Ex 6}: Design a function @tt{number-of-trees=} which given a
@emph{Natural} @tt{h}, returns the number of trees of that height. You should
not have to create the trees (or use @tt{trees=} at all) to complete this
definition. Instead, consider how to generate the number using the same
recursive structure as your solution to @tt{trees=}.
