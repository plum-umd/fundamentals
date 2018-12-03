#lang scribble/manual
@(require scribble/core scribble/examples "helper.rkt"
          (for-label lang/htdp-intermediate-lambda))

@title[#:style 'unnumbered #:tag "20"]{Lab 20: Accumulating Cards}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/intermediate-lam.html"]{Intermediate
Student Language with Lambda}.

Make sure you follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "20:dd"]{Suits, Cards, and Decks}

@#reader scribble/comment-reader (racketblock
;; A Suit is one of: "♠" "♥" "♦" "♣".
(define SUITS '("♠" "♥" "♦" "♣"))
;; A Value is one of: "A" "K" "Q" "J" "10" "9" "8" "7" "6" "5" "4" "3" "2".
(define VALUES '("A" "K" "Q" "J" "10" "9" "8" "7" "6" "5" "4" "3" "2"))
;; A Card is a (string-append Value Suit).
;; A Deck is a list of 52 unique cards in any order.
)

@bold{Ex 1}: Define a constant @tt{DECK} which contains the 52 cards in a
standard deck. Rather than defining the cards by hand, fold over the constants
@tt{SUITS} and @tt{VALUES} to build the deck.

@; (define DECK
@;   (foldr (λ (v d) (foldr (λ (s d) (cons (string-append v s) d)) d SUITS))
@;          '()
@;          VALUES))

@bold{Ex 2}: Design a function @tt{card<?}, which returns @racket[#true] only if
the second card is higher than the first card. To keep things simple, we'll
break ties by suit: @racket["♠"] > @racket["♥"] > @racket["♦"] > @racket["♣"].

@colorize["red"]{@bold{Hint}}: Create a sorted deck using @tt{VALUES} and
@tt{SUITS}, then define a helper-function @tt{index-of} which given an element
and a list, returns the index of that element in the list. The card with the
higher index (or lower index, depending on the order of your sorted deck) is the
higher card.

@; (define card<?
@;   (local [(define (index-of x l)
@;             (cond [(empty? l) (error "Element not in list" x l)]
@;                   [(string=? x (first l)) 0]
@;                   [else (add1 (index-of x (rest l)))]))
@;           (define SORTED
@;             (foldr (λ (v d) (foldr (λ (s d) (cons (string-append v s) d))
@;                                    d
@;                                    SUITS))
@;                    '()
@;                    VALUES))]
@;     (λ (c1 c2) (< (index-of c1 SORTED) (index-of c2 SORTED)))))

@section[#:style 'unnumbered #:tag "20:deal"]{Shuffle Up and Deal}

Swap @bold{Head} and @bold{Hands}!

If we want to play some cards, we first need to shuffle the deck.

@bold{Ex 3}: Define a function @tt{shuffle} which, given a deck of cards,
returns a deck with the same cards as the input but in a random order.

This is a bit tricky, so here are high-level descriptions of two possible
implementation strategies:

@itemlist[

  @item{First @racket[map] the given deck into a @tt{[Listof (list @emph{Number}
        @emph{Card})]} where each card's number is @racket[random]ly generated,
        @racket[sort] the list based on the number, then @racket[map] over the
        sorted list to remove the number.}

  @; (define (shuffle₁ l)
  @;   (map second
  @;        (sort (map (λ (c) (list (random 100000) c)) l)
  @;              (λ (rc sc) (< (first rc) (first sc))))))

  @item{First @racket[random] choose a number between 0 and the @racket[length]
        of the input list of cards. Use @racket[list-ref] to choose that card
        from the input list and add that element to an accumulator,
        @racket[remove] that element from the input list, and continue until the
        input list is empty.}

  @; (define (shuffle₂ l0)
  @;   (local [(define (shuffle/acc l a)
  @;             (cond [(empty? l) a]
  @;                   [else (local [(define c (list-ref l (random (length l))))]
  @;                           (shuffle/acc (remove c l) (cons c a)))]))]
  @;     (shuffle/acc l0 '())))

]

@bold{Ex 4}: Define a function @tt{deal} that given a deck of cards and the
number of players @tt{n}, shuffles and deals the cards to the players by
returning a list of @tt{n} lists of cards.

Example invocations of @tt{deal} and the resulting lists:

@examples[#:label #f
          (eval:alts (deal DECK 1)
                     (eval:result @racketresult[(list (list "3♥" "7♠" "A♦" ... 49 more))]))
          (eval:alts (deal DECK 2)
                     (eval:results
                      (list "(list"
                            @racketresult[(list "4♣" "7♦" "2♥" ... 23 more)]
                            @racketresult[(list "3♣" "Q♣" "K♠" ... 23 more)]
                            ")")))
          (eval:alts (deal DECK 3)
                     (eval:results
                      (list "(list"
                            @racketresult[(list "9♦" "3♣" "7♥" ... 14 more)]
                            @racketresult[(list "8♥" "J♠" "5♣" ... 14 more)]
                            @racketresult[(list "J♥" "2♦" "5♦" ... 15 more)]
                            ")")))
          (eval:alts (deal DECK 4)
                     (eval:results
                      (list"(list"
                            @racketresult[(list "Q♣" "9♠" "K♦" ... 10 more)]
                            @racketresult[(list "Q♦" "J♥" "K♠" ... 10 more)]
                            @racketresult[(list "A♠" "7♣" "5♠" ... 10 more)]
                            @racketresult[(list "9♠" "A♦" "4♣" ... 10 more)]
                            ")")))]
                            

@; (define (deal n)
@;   (local [(define (deal/acc l a)
@;             (cond [(empty? l) a]
@;                   [else (deal/acc (rest l)
@;                                   (append (rest a)
@;                                           `((,(first l) ,@(first a)))))]))]
@;     (deal/acc (shuffle DECK) (build-list n (λ (_) '())))))


@section[#:style 'unnumbered #:tag "20:war"]{This Means @emph{War}}

Swap @bold{Head} and @bold{Hands}!

The card game known as War is relatively simple:

@itemlist[

  @item{each player plays their first card,}

  @item{the player with the highest card adds all cards that were played to the
        bottom of their hand, and}

  @item{the game continues until one person holds all the cards.}

]

@bold{Ex 5}: Design a function @tt{battle} that given a list of N cards, returns
the index of the highest-valued card.

@bold{Ex 6}: Design a function @tt{war} that given the number of players, deals
a random hand to each player and plays the game of @emph{War} until there is a
winner.

@colorize["red"]{@bold{Hint}}: These last two exercises are purposely left
ambiguous. It is up to you to decide how to implement this problem. There is not
one right answer, but some answers are better and simpler than others.
