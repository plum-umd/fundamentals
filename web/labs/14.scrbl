#lang scribble/manual

@(require "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt"
          slideshow/pict
          unstable/gui/pict)

@(define (cell-> c1 c2)
   (hc-append c1
              (text "  ⇒  " null 25)
              c2))

@(define (draw-cells lst)
   (apply
     vc-append
     (for/list ([row lst])
       (apply
         hc-append
         (for/list ([cell row])
           (if (eq? 'live cell)
               (cc-superimpose (rectangle/border 30 30 #:border-width 1)
                               (standard-fish 15 15 #:color "green"))
               (rectangle/border 30 30 #:border-width 1)))))))

@(define exercise (exercise-counter))

@title[#:tag "lab14"]{4/09: Cellular worlds}

@lab:section{Introduction}

In this lab, we will implement something called
@hyperlink["http://en.wikipedia.org/wiki/Cellular_automata"]{cellular automata}. According to
Stephen Wolfram, it is a
@hyperlink["http://en.wikipedia.org/wiki/A_new_kind_of_science"]{a new kind of science}.
More seriously though, a cellular automaton is essentially a grid-based game where
certain cells become "live" according to some simple rules. These rules give rise
to complex behavior despite their simplicity.

If you've heard of @hyperlink["http://en.wikipedia.org/wiki/Turing_machine"]{Turing machine}s
or the @hyperlink["http://en.wikipedia.org/wiki/Lambda_calculus"]{lambda calculus}, cellular
automata can actually be powerful enough to model these too. Theoretically speaking, you can
build automata that can compute anything you could compute using your computer. Then again,
you can do the same thing with
@hyperlink["http://en.wikipedia.org/wiki/Billiard-ball_computer"]{billiard-balls} so maybe
that's not that special.

@lab:section{Warmup}

We will be using the Java World library again in this lab. If you didn't get a chance
to use it in the last lab, take some time to warm yourself up with it.

The World library is on the web
@hyperlink["http://www.ccs.neu.edu/javalib/FunWorld/"]{here}. See the last lab
if you don't remember how to use it.

@exercise{
  As a warm-up, write a world program that represents an @tt{m x m}
  grid of cells that start out blank. When you click a cell, it should activate
  and be colored (choose your favorite color).
}

@lab:section{Conway's Game of Life}

To start out, we will implement a cellular automaton called Conway's Game of Life.
In this game, we start out with a grid of @tt{m x m} cells. Each @racket[Cell] should be
either live or dead. You will choose later what cells start out live or dead.

Cells keep track of its neighboring cells (there are eight neighboring cells) and they
behave differently based on how many of its neighbors are live. If a cell is already live,
then if it has only 0 or 1 live neighbors, it dies. If there are 2 or 3 live neighbors,
it stays alive. Otherwise, it is overcrowded and shrivels.

If the cell is dead to begin with, it will come to life only if there are 3
live neighbors.  Otherwise, the cell stays barren and dead.

In the game, cells update their liveness at each time step or tick.

Here are some examples (you can use these for tests later):

@(cell->
  (draw-cells '((dead live dead)
                (dead live dead)
                (dead dead dead)))
  (draw-cells '((dead dead dead)
                (dead dead dead)
                (dead dead dead))))

@(cell->
  (draw-cells '((dead live dead)
                (dead dead live)
                (dead live dead)))
  (draw-cells '((dead dead dead)
                (dead live dead)
                (dead dead dead))))

@(cell->
  (draw-cells '((live live live)
                (live live live)
                (live live live)))
  (draw-cells '((live dead live)
                (dead dead dead)
                (live dead live))))

@exercise{
  Design an interface, data definition, and class definition for a @racket[Cell].

  You should be able to figure out if a cell is live or dead. Cells should keep track
  of its neighbors and neighbor liveness. You also should be able to either update
  the cell's liveness or produce a new cell with the updated liveness.

  You may also want to have a method that draws a @racket[Cell] for later when you
  build a @racket[World].
}

Now, design a @racket[World] that keeps track of the cells. Make sure to parameterize
the world over the size of the board.

@exercise{
  Design a data and class definition for the @racket[World]. It should keep track of all the
  cells in the game. At each tick, all cells should update their live or dead state
  appropriately.

  For now, have your world start out with some default set of cells live.
}

Make sure that your cells update correctly.

@exercise{
  Add GUI buttons or add key shortcuts to your world that will start, pause,
  and reset the world. Also add the ability to click on a cell and change it from
  live or dead (but only when the game is paused).

  The world should start out paused so you can set the initial cell configuration.
}

@exercise{
  Now that you can set up the board interactively, try various initial configurations.
  Can you find any patterns that do interesting things?
}

@lab:section{Extending the game}

So far, the rules you have implemented are just the plain Game of Life rules.
When you think about it, there's really no reason to hardcode these rules into the
game though. What if we abstracted and made the liveness rules parameters of the game?

@exercise{
  Add parameters to your @racket[World] via its constructor that control how the
  rules of the game work.

  For example, you should be able to set the thresholds at which a cell becomes
  overcrowded and dies. Or the number of live cells that cause a barren cell to
  become live again.
}

@exercise{
  Add either GUI elements or key presses that will change the parameters of the
  game as you run it.
}

Are there any interesting variations on Conway's rules that you can find?

Here are some more ideas that you can try if you've gotten this far:

@exercise{
  Make barren cells randomly come to life with a low probability. Make the
  probability of random mutation tunable.
}

@exercise{
  Add a parameter to your simultation that will change how large a cell's
  neighborhood is. For example, Conway's game of life has a 1-distance neighborhood
  (only immediate neighbors). Make the game able to support 2-distance neighbors (so
  each cell would have 25 neighbors) and so on.

  This parameter affects how your other parameters should behave (like the liveness
  thresholds) so you should keep that in mind when adding it.
}
