#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt"
          (for-label (except-in class/0 define-struct)
                     2htdp/image
                     (only-in lang/htdp-intermediate-lambda define-struct)
                     class/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require class/0))
    (the-eval '(require 2htdp/image))
    ;(the-eval '(require class/universe))
    the-eval))

@(define exercise (exercise-counter))

@title[#:tag "lab04"]{1/28: Universe}



@lab:section{Universe with objects}

The purpose of this lab is to practice writing distributed programs
with Universe.

@exercise{ Starting with the Zombie game we've developed in class,
  develop a single-player distributed version of Zombie.  This game
  should function just like the World version of the game, the only
  difference is that the server will maintain the data of where the
  zombies and the player are.  The client will communicate the mouse
  position to the server as it changes.  The server will communicate
  the location of the player and zombies on a regular basis. }

@exercise{ Modify the above program to make a @emph{multi-player}
  distributed version of Zombie in which the zombies move toward the
  @emph{closest} player in the game.  Each client is responsible for
  moving a single player, but it also displays the position of every
  other player and all of the zombies.
}

It should not be possible for the clients to cheat, for example, by
moving too fast or influencing the position of other players.

@exercise{ Once you have a version of the server that works for
well-behaved clients, fortify your server so that it works for all
possible clients and potential interactions.}
