#lang scribble/manual
@(require "../utils.rkt"
          (for-label class/0))

@title[#:tag "assign02"]{1/18: Space Invaders}

Due: 1/18.

Language: @racketmodname[class/0].

@itemlist[#:style 'ordered 
 @item{@bold{Space Invaders!}
       
        For this exercise, you will design and develop (a pared down
        version of) the classic game of
        @emph{@link["http://en.wikipedia.org/wiki/Space_Invaders"]{Space
        Invaders}}.  In this game, there are a number of space aliens
        that are descending from the top of the screen.  They move
        left to right and then down at uniform speed.  The player
        controls a laser canon that can be moved left or right along
        the bottom of the screen.  The player can fire the laser,
        which shoots straight up.  If the laser hits an alien, the
        alien dies.  If any alien makes it to the bottom of the screen
        (or hits the cannon), the player loses.  If the player
        destroys all the invaders, the player wins.
	
	To get a sense of the game, you can play this
	@link["http://www.freespaceinvaders.org/"]{online version} of
	the game.  Your version doesn't need to have all of the
	features of the online game; in particular, you don't need
	levels, different kinds of aliens, protective bunkers,
	shooting aliens, or the mysterious red alien that flies across
	the top of the screen.  You don't need to keep score and the
	player only needs to have one life.  Of course, @emph{you can}
	implement all of these nice features, but remember, you're
	graded for your program design, not making a cool video game.
	So whatever you add, make sure it's well designed.}

  @item{@bold{Finger exercises: Designing classes}

        Design classes to represent @emph{ternary} trees of numbers.
	(A ternary tree is like a binary tree, except nodes have
	@emph{three} subtrees instead of two.)

        Implement the methods @tt{size}, @tt{sum},
	@tt{prod}, @tt{contains?}, @tt{map}, and @tt{max}.}  ]
