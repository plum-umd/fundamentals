#lang scribble/manual
@(require "../utils.rkt"
          (for-label class/1))

@title[#:tag "assign06"]{2/13: A Computer Pong Player}

Due: 2/13.

Language: @racketmodname[class/1]

Sometimes, all you want is a quick game of Pong, but you can't find
anyone to play with.  In this assignment, we'll solve this problem.  

You need to implement a computer player that plays pong with the
server you wrote @seclink["assign04"]{previously}.  This player will be
a stand-alone world that communicates with the server using just the
same messages that you sent back and forth.  Since that design ensured
that players couldn't cheat, your computer player won't be able to
cheat either.

Your computer player does not need to be perfect, but it should make
reasonably intelligent decisions, such as moving toward the ball.  

Your computer player should effectively play against itself, as well
as against your human player.

