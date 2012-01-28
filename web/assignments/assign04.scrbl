#lang scribble/manual
@(require "../utils.rkt"
          (for-label class/0))

@title[#:tag "assign04"]{2/1: Full Invader}

Due: 2/1, midnight.

Language: @racketmodname[class/0].

Revise your solution to @secref{assign02}.  You should incorporate any
feedback given to you by your graders and make any improvements to the
code you'd like to make.

You will then need to add the following features:

@itemlist[#:style 'ordered

@item{Have at least three different kinds of invaders.  They should
appear different and be worth different amounts when destroyed.  For
faithfulness to the original, one kind should be worth 10, another 20,
and another 30 points each.}

@item{Animate the invaders as they move across the screen; don't have
them render as a single static image.}

@item{Invaders with no invaders below them may now shoot back at the
player.  Invaders should shoot randomly at some frequency.}

@item{If all the invaders are dead, a new level should start.  The new
level should be more difficult in that invaders should shoot with a
higher frequency than the previous level.  There should be no limit on
the number of levels played in a game.}

@item{The laser should be able to shoot only one shot at a time; it
cannot shoot again until the shot it fired hits something or reaches
the top of the screen.}

@item{Maintain and display a running score for the player.}

@item{Include a ``mystery'' invader that periodically flies
horizontally across the top of the screen.  If hit, it is worth either
100, 150, or 300 points, chosen randomly.}

@item{A player should start with 3 lives.  Every time their laser is
hit by an invader's shot, they lose a life.  The game is now over when
the player has no lives or the invaders make it to the ground.  The
game should display how many lives the player has left.}

@item{The game should start with three protective bunkers that sit in
between the invaders and the laser, providing partial coverage of the
laser.  Once shot 5 times, either by the invaders or the player, the
bunker is destroyed.}]





