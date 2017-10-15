#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner))
@(define (colorize c . content)
   (elem #:style (style #f (list (color-property c)))
         content))

@title[#:style 'unnumbered #:tag "snake"]{The Snake Game}

@section[#:style 'unnumbered #:tag "snake:changes"]{What things change over time in the snake game?}

Per the discussion in the previous lecture:

@itemlist[
  @item{the length of the snake}
  @item{the placement of the food}
  @item{the x, y of the head of the snake}
  @item{the score}
  @item{when the snake turns}
  @item{position of each segment of the snake's body}
  @item{if the snake is eating food or not}
  @item{when the food is produced}
  @item{when you lose}
]

For each of the changes that were listed out on Wednesday, which information can
be computed based on other values and which are necessary to remember?

@section[#:style 'unnumbered #:tag "snake:change-discussion"]{Things that Change}

Many of the listed "things that change" do not have to be directly represented.

@itemlist[
  @item{@colorize["red"]{✗} The length of the snake @colorize["red"]{can be
        computed};}
  @item{@colorize["green"]{✓} the placement of the food @colorize["green"]{must
        be remembered};}
  @item{@colorize["red"]{✗} the x, y of the head of the snake
        @colorize["red"]{is no different from other segments};}
  @item{@colorize["red"]{✗} the score @colorize["red"]{will be computed as the
        length of the snake};}
  @item{@colorize["green"]{✓} when the snake turns @colorize["green"]{or rather,
        the direction the snake is moving};}
  @item{@colorize["green"]{✓} position of each segment of the snake's body
        @colorize["green"]{must be remembered};}
  @item{@colorize["red"]{✗} if the snake is eating food or not
        @colorize["red"]{is known based on the location of the snake's head and
                         the locations of the food};}
  @item{@colorize["red"]{✗} when the food is produced @colorize["red"]{will
        occur with some constant probability}; and}
  @item{@colorize["red"]{✗} when you lose @colorize["red"]{is known based on the
        location of the snake's segments and the game borders}.}
]

So, what's left?

@itemlist[
  @item{the X, Y of all food must be known,}
  @item{the direction in which the snake moves must be known, and}
  @item{the X, Y of every segment of the snake must be known.}
]

The only thing that distinguishes one block of food from another is its
location, so, let's implement @emph{Food} as @emph{Posn}s.

@#reader scribble/comment-reader (racketblock
;; A Food is a (make-posn Int Int)
;; Interp: the location of a block of food in the game.

;; food-template : Food -> ???
(define (food-template f)
  (... (posn-x f) ... (posn-y f) ...))
)

There can be an arbitrary amount of food on the game board, so...

@#reader scribble/comment-reader (racketblock
;; A ListofFood is one of:
;; - '()
;; - (cons Food ListofFood)
;; Interp: an arbitrary amount of Food.

;; foods-template : ListofFood -> ???
(define (foods-template fs)
  (cond [(empty? fs) ...]
        [(cons? fs) (... (first fs)
                         ...
                         (foods-template (rest fs)))]))
)

@bold{Note}: You should write examples with every data definition to be used in
tests. Doing so ahead of time has two benefits: it will
@itemlist[
  @item{force you to understand the data you're using in your implementation and}
  @item{make writing tests for your functions much faster.}
]

The @emph{Snake} can move in one of four directions, so we use four strings to
distinguish these directions.

@#reader scribble/comment-reader (racketblock
;; A Dir is one of: "left", "right", "up", or "down".
;; Interp: the direction in which the head of the snake is moving.

;; dir-template : Dir -> ???
(define (dir-template d)
  (cond [(string=? "left"  d) ...]
        [(string=? "right" d) ...]
        [(string=? "up"    d) ...]
        [(string=? "down"  d) ...]))
)

We could use any four distinct values for this purpose, such as one of four
numbers {@racket[1], @racket[2], @racket[3], @racket[4]}, or one of four
distinct @emph{Posn}s {@racket[(make-posn 0 1)], @racket[(make-posn 1 0)],
@racket[(make-posn -1 0)], @racket[(make-posn 0 -1)]}). Think a bit about why
are some choices better than others.

With the choice below, not only is the direction obvious at a glance, but each
@emph{Dir} happens to be a valid @emph{KeyEvent}. This may end up being useful
as we design the rest of the program.

Each segment of the snake has a location, so let's use @emph{Posn}s again to
represent each @emph{Seg} in the snake.

@#reader scribble/comment-reader (racketblock
;; A Seg is (make-posn Int Int)
;; Interp: the location of one piece of the snake

;; seg-template : Seg -> ???
(define (seg-template s)
  (... (posn-x s) ... (posn-y s) ...))
)

A snake may be arbitrarily long, so we'll need some way to represent an
arbitrary number of segments. But what's the smallest number of @emph{Seg}s that
constitute a snake? It must start with at least one, so we need a data
definition that contains one or more @emph{Seg}s. Per the definition below, it's
impossible to create a @emph{NEListofSeg} with no @emph{Seg}s in it.

@#reader scribble/comment-reader (racketblock
;; A NEListofSeg is one of:
;; - (cons Seg '())
;; - (cons Seg NEListofSeg)
;; Interp: the segments of the snake in the game, where the first is the snake's
;; head.
)

This is different than other lists we've seen, since all other lists allowed a
list of zero-length. When writing our template for any functions that operate on
@emph{NEListofSeg}s, we'll need to distinguish these two cases using something
other than @racket[empty?]. How is the first case in the @emph{NEListofSeg} data
definition different from the second? Its @racket[rest] is @racket['()]!

@#reader scribble/comment-reader (racketblock
;; segs-template : NEListofSeg -> ???
(define (segs-template ss)
  (cond [(empty? (rest ss)) (... (first ss) ...)]
        [(cons?  (rest ss)) (... (first ss)
                                 ...
                                 (segs-template (rest ss)))]))
)

Now we can give a data definition for a @emph{Snake}:

@#reader scribble/comment-reader (racketblock
;; A Snake is a (make-snake NEListofSeg Dir)
;; Interp: All of the segments inside the snake and the direction
;; that the snake is moving.
(define-struct snake (segs dir))

;; snake-template : Snake -> ???
(define (snake-template s)
  (... (segs-template (snake-segs s)) ... 
       (dir-template (snake-dir s)) ...))
)


This is everything we need in the @emph{WorldState} of our @emph{Game}:

@#reader scribble/comment-reader (racketblock
;; A Game is a (make-game Snake ListofFood)
;; Interp: the WorldState of the snake game.
(define-struct game (snake foods))
)

As with all composite data, given a @emph{Game} we can tear it apart. Each of
@emph{Snake} and @emph{ListofFood} have their own templates. Explicitly applying
those templates will give hints for where we should expect to need
helper-functions when working with @emph{Game}s.

@#reader scribble/comment-reader (racketblock
;; game-template : Game -> ???
(define (game-template g)
  (... (snake-template (game-snake g)) ...
       (foods-template (game-foods g)) ...))
)

@emph{Block}s help us scale the game board to any size we want and simplify
other operations. The @emph{Snake} moves a single @emph{Block} each tick. Each
of the @emph{Seg}s and @emph{Food} are a single @emph{Block}. Each block is
rendered as a square @racket[BLOCK-SCALE] pixels wide.

@#reader scribble/comment-reader (racketblock
;; A Block is a (make-posn Int Int)
;; Interp: the smallest unit of measure on the game board.
)

With these data definitions, we can implement the operations for our snake game.


@section[#:style 'unnumbered #:tag "snake:examples"]{Aside: defining examples}

@bold{Every single student} I interact with during office hours does not test
their functions. The core reason is that writing tests for the function is the
hardest part of function design. @bold{There is a reason why writing tests is
hard.}

To write a test for a function, one needs to know:
@itemlist[
  @item{how to construct valid input values,}
  @item{how to construct a valid output value, and}
  @item{how the input values are used to create the output value}
]

If you've actually followed the design recipe when you were creating your data
definitions, you've already created a bunch of valid input values for any
function that operates those data definitions. This lets you focus on the
important part of the test: the expected output, and particularly, how the input
is used to create the expected output.

Here are some simple examples of @emph{Game}s, which requires examples of each
data definition we've created.

@#reader scribble/comment-reader (racketblock
(define DIR0 "down")
(define DIR1 "right")
(define DIR2 "up")

(define SEG0 (make-posn 0 0))
(define SEG1 (make-posn 0 1))
(define SEG2 (make-posn 1 1))

(define SEGS0 (list SEG0))
(define SEGS1 (cons SEG1 SEGS0))
(define SEGS2 (cons SEG2 SEGS1))

(define SNAKE0 (make-snake SEGS0 DIR0))
(define SNAKE1 (make-snake SEGS1 DIR1))
(define SNAKE2 (make-snake SEGS2 DIR2))

(define FOOD0 (make-posn 1 1))
(define FOOD1 (make-posn 2 2))
(define FOOD2 (make-posn 3 2))

(define FOODS0 '())
(define FOODS1 (cons FOOD0 FOODS0))
(define FOODS2 (cons FOOD1 FOODS1))
(define FOODS3 (cons FOOD2 FOODS2))

(define GAME0 (make-game SNAKE0 FOODS0))
;; eating on next turn:
(define GAME1 (make-game SNAKE1 FOODS1))
(define GAME2 (make-game SNAKE2 FOODS2))
;; collision with self:
(define GAMEOVER0 (make-game (make-snake (list* (make-posn 0 0)
                                                (make-posn 1 0)
                                                SEGS2)
                                         "left")
                             FOODS3))
;; collision with wall:
(define GAMEOVER1 (make-game (make-snake (cons (make-posn -1 0) SEGS0) "left")
                             FOODS2))
)


@section[#:style 'unnumbered #:tag "snake:top-down"]{Top-down design of the snake game}

Where do we start? At the top! How? The same way as always, assume we've got
functions that do exactly what we want, then design them.

We'll use @racket[big-bang], so let's start working out that main function to
help us design the program as a whole. We have to give @racket[big-bang] our
initial @emph{Game} as input. We'll need a way @tt{to-draw} each @emph{Game}.
Things that change over time occur @tt{on-tick}: including snake movement, snake
growth, and food generation. The snake's direction changes @tt{on-key}
events. The @emph{Game} should @tt{stop-when} the snake collides with itself or
the edges of the scene.

@#reader scribble/comment-reader (racketblock
;; main : Game -> Game
;; Run the snake game using `big-bang' given the initial world state.
(define (main g)
  (big-bang g                          ; (Game)
            [to-draw   draw-game]      ; (Game -> Image)
            [on-tick   tock-game 1/4]  ; (Game -> Game)
            [on-key    keyh-game]      ; (Game KeyEvent -> Game)
            [stop-when stop-game       ; (Game -> Boolean)
                       game-over]))    ; (Game -> Image)
)

We know what the signatures of these functions must be from the documentation of
@racket[big-bang]. So, let's implement them!


@section[#:style 'unnumbered #:tag "snake:to-draw"]{Drawing the Game}

@emph{Block}s make drawing the game quite easy. We draw each @emph{Block} as a
square @tt{BLOCK-SCALE} pixels wide, so the only function that actually deals
with pixel values is @tt{draw-block-on}.

@bold{Note}: Our drawing functions always take as input the scene on which new images
should be placed.

@#reader scribble/comment-reader (racketblock
(define BLOCK-SCALE 10) ; Block -> Pixel scale
(define MTW 40)   ; Game board width  (# blocks)
(define MTH 30)   ; Game board height (# blocks)
(define MT (rectangle (* (+ 1 MTW) BLOCK-SCALE)
                      (* (+ 1 MTH) BLOCK-SCALE)
                      "solid"
                      "black"))

;; draw-block-on : Block Color Image -> Image
;; Places a square of color C at the scaled pixel
;; location denoted by B on SCN.
(define (draw-block-on b c scn)
  (place-image (square BLOCK-SCALE "solid" c)
               (* (+ 1 (posn-x b)) BLOCK-SCALE)
               (* (+ 1 (posn-y b)) BLOCk-SCALE)
               scn))

;; draw-block-on : LoBlock Color Image -> Image
;; Places a square of color C at the scaled pixel
;; location denoted by B on SCN.
(define (draw-blocks-on bs c scn)
  (cond [(empty? bs) scn]
        [(cons? bs)
         (draw-blocks-on (rest bs)
                         c
                         (draw-block-on (first bs) c scn))]))

(define SEGCOLOR "yellow")
(define FOODCOLOR "red")

;; draw-game : Game -> Image
;; Render the snake game as an image.
(define (draw-game g)
  (draw-blocks-on (snake-segs (game-snake g)) SEGCOLOR
                  (draw-blocks-on (game-foods g) FOODCOLOR MT)))

(define GAMEOVER-SIZE 32)
(define GAMEOVER-COLOR "red")
(define GAMEOVER-MSG "Final score: ")

;; game-score : Game -> Natural
;; Calculate the score of the game as the number of snake segments.
(define (game-score g)
  (length (snake-segs (game-snake g))))

(check-expect (game-score GAME0) 1)
(check-expect (game-score GAME1) 2)
(check-expect (game-score GAME2) 3)

;; game-over : Game -> Image
;; Display the score once we stop the world.
(define (game-over g)
  (overlay (text (string-append GAMEOVER-MSG
                                (number->string (game-score g)))
                 GAMEOVER-SIZE
                 GAMEOVER-COLOR)
           (draw-game g)))

(check-expect (game-over GAME0)
              (overlay (text (string-append GAMEOVER-MSG "1")
                             GAMEOVER-SIZE
                             GAMEOVER-COLOR)
                       (draw-game GAME0)))
(check-expect (game-over GAME1)
              (overlay (text (string-append GAMEOVER-MSG "2")
                             GAMEOVER-SIZE
                             GAMEOVER-COLOR)
                       (draw-game GAME1)))
(check-expect (game-over GAME2)
              (overlay (text (string-append GAMEOVER-MSG "3")
                             GAMEOVER-SIZE
                             GAMEOVER-COLOR)
                       (draw-game GAME2)))
)


@section[#:style 'unnumbered #:tag "snake:on-tick"]{Ticking the Game}

Recall what changes with time:
@itemlist[
  @item{The snake grows if it's eating or}
  @item{moves if it isn't, and}
  @item{food appears randomly at the snake's tail (~20% of the time).}
]

The snake is eating if in the next game state, its head @emph{Seg} is
overlapping with some @emph{Food} on the game board. Assuming that
@tt{next-head} gives us the next head @emph{Seg} of the snake, we've can
implement @tt{eating?}:

@#reader scribble/comment-reader (racketblock
;; eating? : Snake ListofFood -> Boolean
;; Is the snake eating food in the next state?
(define (eating? s fs)
  (member? (next-head s) fs))

(check-expect (eating? SNAKE0 FOODS0) #false)
(check-expect (eating? SNAKE1 FOODS1) #true)
(check-expect (eating? SNAKE2 FOODS2) #false)
)

So, we need to design @tt{next-head} as well. We've already decided that the
next head will be one @emph{Block} away from the @tt{current-head}. The current
head of the snake is easy:

@#reader scribble/comment-reader (racketblock
;; current-head : Snake -> Seg
;; Return the head segment of the given snake.
(define (current-head s)
  (first (snake-segs s)))

(check-expect (current-head SNAKE0) SEG0)
(check-expect (current-head SNAKE1) SEG1)
(check-expect (current-head SNAKE2) SEG2)
)

So, we just need to know in which @emph{Dir} that one @emph{Block} should be
added to implement @tt{next-head}:

@#reader scribble/comment-reader (racketblock
;; next-head : Snake -> Seg
;; Return the next head segment of the given snake.
(define (next-head s)
  (cond [(string=? "left" (snake-dir s))
         (make-posn (- (posn-x (current-head s)) 1)
                    (posn-y (current-head s)))]
        [(string=? "right" (snake-dir s))
         (make-posn (+ 1 (posn-x (current-head s)))
                    (posn-y (current-head s)))]
        [(string=? "down" (snake-dir s))
         (make-posn (posn-x (current-head s))
                    (+ 1 (posn-y (current-head s))))]
        [(string=? "up" (snake-dir s))
         (make-posn (posn-x (current-head s))
                    (- (posn-y (current-head s)) 1))]))

(check-expect (next-head SNAKE0) (make-posn (posn-x (current-head SNAKE0))
                                            (+ 1 (posn-y (current-head SNAKE0)))))
(check-expect (next-head SNAKE1) (make-posn (+ 1 (posn-x (current-head SNAKE1)))
                                            (posn-y (current-head SNAKE1))))
(check-expect (next-head SNAKE2) (make-posn (posn-x (current-head SNAKE2))
                                            (- (posn-y (current-head SNAKE2)) 1)))
)

Now we know how to test if the snake is @tt{eating?} on the next tick. On each
tick the snake's @tt{next-head} will be added to the snake's
@emph{ListofSeg}. If the snake is not @tt{eating?} its last @emph{Seg} is
removed; if the snake is @tt{eating?} it is not.

The function @tt{next-snake} implements this behavior with the help of
@tt{move-snake} and @tt{grow-snake}, and @tt{remove-last}.

Note the signature @tt{remove-last : NEListofSeg -> ListofSeg}. If
@tt{remove-last} is given a single-element list it will return the empty list,
so it would be incorrect to give @emph{NEListofSeg} as the return type. But once
we add the @tt{next-head} to the @emph{ListofSeg}, we once again know we're
satisfying @tt{make-snake}'s signature.

@#reader scribble/comment-reader (racketblock
;; next-snake : Snake ListofFood -> Snake
;; Create the next snake given the last snake and the existing food.
(define (next-snake s fs)
  (cond [(eating? s fs) (grow-snake s)]
        [else (move-snake s)]))
(check-expect (next-snake SNAKE0 FOODS0)
              (make-snake (list SEG1) DIR0))
(check-expect (next-snake SNAKE1 FOODS1) (make-snake SEGS2 DIR1))
(check-expect (next-snake SNAKE2 FOODS2)
              (make-snake (list (make-posn 1 0) (make-posn 1 1) (make-posn 0 1))
                          DIR2))

;; move-snake : Snake -> Snake
;; Creates a new snake with the next head and the last segment removed.
(define (move-snake s)
  (make-snake (cons (next-head s) (remove-last (snake-segs s)))
              (snake-dir s)))
(check-expect (move-snake SNAKE0) (make-snake (list SEG1) DIR0))
(check-expect (move-snake SNAKE1) (make-snake (list SEG2 SEG1) DIR1))
(check-expect (move-snake SNAKE2)
              (make-snake (list (make-posn 1 0) (make-posn 1 1) (make-posn 0 1))
                          DIR2))

;; grow-snake : Snake -> Snake
;; Creates a new snake with the next head added.
(define (grow-snake s)
  (make-snake (cons (next-head s) (snake-segs s))
              (snake-dir s)))
(check-expect (grow-snake SNAKE0) (make-snake SEGS1 DIR0))
(check-expect (grow-snake SNAKE1) (make-snake SEGS2 DIR1))
(check-expect (grow-snake SNAKE2)
              (make-snake (cons (make-posn 1 0) SEGS2)
                          DIR2))

;; remove-last : NELoSeg -> LoSeg
;; Removes the last element from the given NELoSeg.
;; Note: the returned list may not be non-empty.
(define (remove-last ss)
  (cond [(empty? (rest ss)) '()]
        [else (cons (first ss) (remove-last (rest ss)))]))

(check-expect (remove-last SEGS0) '())
(check-expect (remove-last SEGS1) (list (make-posn 0 1)))
(check-expect (remove-last SEGS2) (list (make-posn 1 1) (make-posn 0 1)))
)

Finally, we have to handle @emph{Food} generation. We've decided to generate
food about once every 5 moves of the snake, and we'll define a value to easily
configure that parameter. If we add food, we'll add it on the tail of the snake.

@#reader scribble/comment-reader (racketblock
;; maybe-add-food : Snake ListofFood -> ListofFood
;; Add food under the tail of the snake about FOOD-FREQ⁻¹ often.
(define FOOD-FREQ 5)
(define (maybe-add-food s fs)
  (cond [(= 0 (random FOOD-FREQ))
         (cons (return-last (snake-segs s)) fs)]
        [else fs]))

(check-random (maybe-add-food SNAKE0 FOODS0)
              (cond [(= 0 (random FOOD-FREQ)) (cons SEG0 FOODS0)]
                    [else FOODS0]))
(check-random (maybe-add-food SNAKE1 FOODS1)
              (cond [(= 0 (random FOOD-FREQ)) (cons SEG0 FOODS1)]
                    [else FOODS1]))
(check-random (maybe-add-food SNAKE2 FOODS2)
              (cond [(= 0 (random FOOD-FREQ)) (cons SEG0 FOODS2)]
                    [else FOODS2]))

;; return-last : NELoSeg -> Seg
;; Returns the last element.
(define (return-last ss)
  (cond [(empty? (rest ss)) (first ss)]
        [else (return-last (rest ss))]))

(check-expect (return-last SEGS0) SEG0)
(check-expect (return-last SEGS1) SEG0)
(check-expect (return-last (reverse SEGS2)) SEG2)

;; next-foods : Snake ListofFood -> ListofFood
(define (next-foods s fs)
  (maybe-add-food s (remove (next-head s) fs)))

(check-random (next-foods SNAKE0 FOODS0)
              (maybe-add-food SNAKE0 (remove (next-head SNAKE0) FOODS0)))
(check-random (next-foods SNAKE1 FOODS1)
              (maybe-add-food SNAKE1 (remove (next-head SNAKE1) FOODS1)))
(check-random (next-foods SNAKE2 FOODS2)
              (maybe-add-food SNAKE2 (remove (next-head SNAKE2) FOODS2)))
)

This was the last piece we need to add to implement @tt{tock-game}.

@#reader scribble/comment-reader (racketblock
;; tock-game : Game -> Game
;; Change the state of the snake game after one clock tick.
(define (tock-game g)
  (make-game (next-snake (game-snake g) (game-foods g))
             (next-foods (game-snake g) (game-foods g))))

(check-random (tock-game GAME0) ; not eating
              (make-game (move-snake SNAKE0) (maybe-add-food SNAKE0 FOODS0)))
(check-random (tock-game GAME1) ; eating
              (make-game (grow-snake SNAKE1)
                         (maybe-add-food SNAKE1 (remove (next-head SNAKE1) FOODS1))))
(check-random (tock-game GAME2) ; not eating
              (make-game (move-snake SNAKE2) (maybe-add-food SNAKE2 FOODS2)))
)


@section[#:style 'unnumbered #:tag "snake:stop-when"]{Stopping the Game}

The snake game ends when the snake collides with itself or if it goes off the
game board.

@#reader scribble/comment-reader (racketblock
;; on-board? : Seg -> Boolean
;; Returns true only if the given segment is on the game board.
(define (on-board? s)
  (and (>= (posn-x s) 0)   (>= (posn-y s) 0)
       (<= (posn-x s) MTW) (<= (posn-y s) MTH)))
(check-expect (on-board? SEG0) #true)
(check-expect (on-board? SEG1) #true)
(check-expect (on-board? SEG2) #true)
(check-expect (on-board? (make-posn -1 0)) #false)
(check-expect (on-board? (make-posn 0 -1)) #false)
(check-expect (on-board? (make-posn (+ MTW 1) 0)) #false)
(check-expect (on-board? (make-posn 0 (+ MTH 1))) #false)
(check-expect (on-board? (make-posn -1 (+ MTH 1))) #false)

;; stop-game : Game -> Boolean
;; Stop the snake game when
;; - the snake collides with the wall or
;; - the snake collides with itself.
(define (stop-game g)
  (or (member? (current-head (game-snake g))
               (rest (snake-segs (game-snake g))))
      (not (on-board? (current-head (game-snake g))))))

(check-expect (stop-game GAME0) #false)
(check-expect (stop-game GAME1) #false)
(check-expect (stop-game GAME2) #false)
(check-expect (stop-game GAMEOVER0) #true)
(check-expect (stop-game GAMEOVER1) #true)
)


@section[#:style 'unnumbered #:tag "snake:on-key"]{Controlling the Game}

We want the snake to move with the arrow keys. Per the
@link["https://docs.racket-lang.org/teachpack/2htdpuniverse.html"]{documentation},
the @emph{KeyEvent}s associated with the arrow keys are precisely
@emph{Dir}s. This makes the @tt{keyh-game} quite easy to implement.

@#reader scribble/comment-reader (racketblock
;; keyh-game : Game KeyEvent -> Game
;; Handle big-bang key events for the snake game.
(define (keyh-game g ke)
  (cond [(or (string=? ke "left") (string=? ke "right")
             (string=? ke "down") (string=? ke "up"))
         (make-game (make-snake (snake-segs (game-snake g)) ke)
                    (game-foods g))]
        [else g]))

(check-expect (keyh-game GAME0 "right")
              (make-game (make-snake SEGS0 "right") FOODS0))
(check-expect (keyh-game GAME1 "left")
              (make-game (make-snake SEGS1 "left") FOODS1))
(check-expect (keyh-game GAME2 "down")
              (make-game (make-snake SEGS2 "down") FOODS2))
)

Now we can run our snake game by passing any example game to @tt{main}.
