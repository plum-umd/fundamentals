;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname pac0-soln) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define (main _)
  (big-bang GAME0
    [on-draw world-draw]
    [on-tick world-tick 1/30]
    [on-key world-key]))

;; -----------------------------------------------------------------------------
;; Data Definitions

;; A World is one of:
;; - a Game
;; - a Number
;; Interp: game representes an ongoing game, number represents final score
#;
(define (world-template w)
  (cond [(number? w) ...]
        [(game? w) (game-template w)]))

;; A Game is a (make-game Pac Dots Number)
(define-struct game (pac dots score))
#;
(define (game-template g)
  (... (pac-template (game-pac g))
       (dots-template (game-dots g))
       (game-score g) ...))

;; A PacMan is a (make-pac Dir Dir Loc)
(define-struct pac (dir next loc))
#;
(define (pac-template pm)
  (... (dir-template (pac-dir pm))
       (dir-template (pac-next pm))
       (loc-templtae (pac-loc pm)) ...))

;; A Dots is one of:
;; - '()
;; - (cons Dot Dots)
#;
(define (dots-template ds)
  (cond [(empty? ds) ...]
        [(cons? ds)
         (... (first ds)
              (dots-template (rest ds)) ...)]))

;; A Dot is a (make-posn Integer Integer)
;; Assume: x ∈ [0,WIDTH), y ∈ [0,HEIGHT)
;; Interp: grid coordinates.

;; A Dir is one of:
;; - "left"
;; - "right"
;; - "up"
;; - "down"
#;
(define (dir-template d)
  (cond [(string=? d "left") ...]
        [(string=? d"right") ...]
        [(string=? d "up")   ...]
        [(string=? d "down") ...]))

;; A Loc is a (make-posn Rational Rational)
;; Assume: x ∈ [0,WIDTH), y ∈ [0,HEIGHT)
;; Interp: grid coordinates.

;; A Px is a (make-posn Integer Integer)
;; Interp: px coordinates


;; -----------------------------------------------------------------------------
;; Constants

(define PAC-SPEED 1/3) ; grid units per tick
(define GRID-SIZE 30)  ; px per grid unit
(define WIDTH  10)     ; grid units
(define HEIGHT 10)     ; grid units

(define DOTS0
  (list (make-posn 0 0)
        (make-posn (sub1 WIDTH) (sub1 HEIGHT))))

(define PAC0 (make-pac "down" "down" (make-posn 1 1)))

(define GAME0 (make-game PAC0 DOTS0 0))

(define PAC-IMG
  (circle (* 1/2 GRID-SIZE) "solid" "yellow"))

(define DOT-IMG
  (circle (* 1/8 GRID-SIZE) "solid" "black"))

(define SCENE
  (rectangle (* GRID-SIZE WIDTH)
             (* GRID-SIZE HEIGHT)
             "solid"
             "white"))


;; -----------------------------------------------------------------------------
;; World functions

;; world-tick : World -> World
;; If game is ongoing, tick game otherwise stop
(check-expect (world-tick GAME0) (game-tick GAME0))
(check-expect (world-tick 5) (stop-with 5))             
(define (world-tick w)
  (cond [(number? w) (stop-with w)]
        [(game? w) (game-tick w)]))

;; world-draw : World -> Scene
;; Render the world as a scene
(check-expect (world-draw GAME0) (game-draw GAME0))
(check-expect (world-draw 5) (score-draw 5))
(define (world-draw w)
  (cond [(number? w) (score-draw w)]
        [(game? w) (game-draw w)]))

;; world-key : World KeyEvent -> World
;; If game is ongoing, handle key otherwise stop
(check-expect (world-key GAME0 "down") (game-key GAME0 "down"))
(check-expect (world-key 5 "n") (stop-with 5))
(define (world-key w ke)
  (cond [(number? w) (stop-with w)]
        [(game? w) (game-key w ke)]))

;; -----------------------------------------------------------------------------
;; Game functions

;; game-tick : Game -> Game
;; Advance the pacman game
(check-expect (game-tick GAME0) (game-pac-tick GAME0))
(check-expect (game-tick (make-game PAC0 '() 0))
              (game-advance-level (make-game PAC0 '() 0)))
(define (game-tick g)
   (if (game-level-over? g)
       (game-advance-level g)
       (game-pac-tick g)))

;; game-level-over? : Game -> Boolean
;; Are all the dots gone in this level?
(check-expect (game-level-over? (make-game PAC0 DOTS0 0)) #false)
(check-expect (game-level-over? (make-game PAC0 '() 0)) #true)
(define (game-level-over? g)
  (empty? (game-dots g)))

;; game-advance-level : Game -> Game
;; Advance to the next level by resetting dots
(check-expect (game-advance-level (make-game PAC0 '() 0))
              (make-game PAC0 DOTS0 0))
(define (game-advance-level g)
  (make-game (game-pac g)
             DOTS0
             (game-score g)))

;; game-pac-tick : Game -> Game
;; Advance pacman within game
(check-expect (game-pac-tick (make-game PAC0 DOTS0 0))
              (make-game (pac-tick PAC0)
                         (pac-eat-dots PAC0 DOTS0)
                         0))
(define (game-pac-tick g)
  (make-game (pac-tick (game-pac g))
             (pac-eat-dots (game-pac g) (game-dots g))
             (game-new-score g)))

;; game-new-score : Game -> Number
;; Compute new score in game (+1 for eating a dot)
(check-expect (game-new-score (make-game PAC0 '() 0)) 0)
(check-expect (game-new-score (make-game PAC0 (list (pac-loc PAC0)) 0)) 1)
(define (game-new-score g)
  (+ (game-score g)
     (if (pac-eating-dots? (game-pac g) (game-dots g)) 1 0)))

;; game-set-dir : Game Dir -> Game
;; Set the next direction for pacman in given game
(check-expect (game-set-dir GAME0 "down")
              (make-game (pac-set-next-dir PAC0 "down") DOTS0 0))
(define (game-set-dir g d)
  (make-game (pac-set-next-dir (game-pac g) d)
             (game-dots g)
             (game-score g)))

;; world-key : Game KeyEvent -> World
;; Handle arrow keys by setting pac direction, end game on esc, ignore others
(check-expect (game-key GAME0 "down") (game-set-dir GAME0 "down"))
(check-expect (game-key GAME0 "escape") 0)
(check-expect (game-key GAME0 "!") GAME0)
(define (game-key g ke)
  (cond [(arrow? ke) (game-set-dir g ke)]
        [(key=? ke "escape") (game-score g)]
        [else g]))

;; arrow? : KeyEvent -> Boolean
;; Is the given key event an arrow key?
(check-expect (arrow? "down") #true)
(check-expect (arrow? "1") #false)
(define (arrow? ke)
  (or (key=? ke "left")
      (key=? ke "right")
      (key=? ke "up")
      (key=? ke "down")))

;; -----------------------------------------------------------------------------
;; PacMan functions

;; pac-tick : PacMan -> PacMan
;; Tick pacman along: move, then update new direction
(check-expect (pac-tick PAC0) (pac-new-dir (pac-move PAC0)))
(define (pac-tick pm)
  (pac-new-dir (pac-move pm)))

;; pac-new-dir : PacMan -> PacMan
;; Change to new direction if grid aligned
(check-expect (pac-new-dir (make-pac "right" "down" (make-posn 0 0)))
              (make-pac "down" "down" (make-posn 0 0)))
(check-expect (pac-new-dir (make-pac "right" "down" (make-posn 1/2 0)))
              (make-pac "right" "down" (make-posn 1/2 0)))
(define (pac-new-dir pm)
  (if (loc-grid-aligned? (pac-loc pm))
      (make-pac (pac-next pm)
                (pac-next pm)
                (pac-loc pm))
      pm))

;; pac-move : PacMan -> PacMan
;; Move given pacman in its current direction (stuck at walls)
(check-expect (pac-move (make-pac "right" "right" (make-posn 0 0)))
              (make-pac "right" "right" (make-posn PAC-SPEED 0)))
(check-expect (pac-move (make-pac "left" "left" (make-posn 0 0))) ; wall
              (make-pac "left" "left" (make-posn 0 0)))
(define (pac-move pm)
  (make-pac (pac-dir pm)
            (pac-next pm)
            (dir-move-loc (pac-dir pm) (pac-loc pm) PAC-SPEED)))

;; pac-set-next-dir : PacMan Dir -> PacMan
;; Set pac's next direction
(check-expect (pac-set-next-dir (make-pac "left" "down" (make-posn 0 0)) "up")
              (make-pac "left" "up" (make-posn 0 0)))
(define (pac-set-next-dir p d)
  (make-pac (pac-dir p) d (pac-loc p)))

;; pac-eat-dots : PacMan Dots -> Dots
;; Remove dots eaten by given pacman
(define (pac-eat-dots pm ps)
  (dots-remove-loc ps (pac-loc pm)))

;; pac-eat-dots? : PacMan Dots -> Boolean
;; Is pac eating any of the dots?
(define (pac-eating-dots? pm ps)
  (dots-contains-loc? ps (pac-loc pm)))

;; -----------------------------------------------------------------------------
;; Dots functions

;; dots-remove-loc : Dots Loc -> Dots
;; Remove any dots equal to given location
(check-expect (dots-remove-loc '() (make-posn 0 0)) '())
(check-expect (dots-remove-loc (list (make-posn 1 1) (make-posn 0 0))
                               (make-posn 0 0))
              (list (make-posn 1 1)))
(define (dots-remove-loc ps l)
  (cond [(empty? ps) '()]
        [(cons? ps)
         (if (loc=? l (first ps))
             (dots-remove-loc (rest ps) l)
             (cons (first ps)
                   (dots-remove-loc (rest ps) l)))]))

;; dots-contains-loc? : Dots Loc -> Boolean
;; Do the dots contain the given location?
(check-expect (dots-contains-loc? '() (make-posn 0 0)) #false)
(check-expect (dots-contains-loc? (list (make-posn 1 1) (make-posn 0 0))
                                  (make-posn 0 0))
              #true)
(define (dots-contains-loc? ps l)
  (cond [(empty? ps) #false]
        [(cons? ps)
         (or (loc=? l (first ps))
             (dots-contains-loc? (rest ps) l))]))


;; -----------------------------------------------------------------------------
;; Dir functions

;; dir-move-loc : Dir Loc Scale -> Loc
;; Move location in given direction at given speed (stuck at walls)
(check-expect (dir-move-loc "left" (make-posn 0 0) 1)
              (make-posn 0 0))
(check-expect (dir-move-loc "right" (make-posn 0 0) 1)
              (make-posn 1 0))
(define (dir-move-loc d l s)
  (loc+ l (loc-scale (dir->delta d) s)))

;; dir->delta : Dir -> Loc
;; Convert a direction to a (change in) location at PacMan's speed
(check-expect (dir->delta "left")  (make-posn -1 0))
(check-expect (dir->delta "right") (make-posn  1 0))
(check-expect (dir->delta "up")    (make-posn  0 -1))
(check-expect (dir->delta "down")  (make-posn  0  1))
(define (dir->delta dir)
  (cond [(string=? dir "left")  (make-posn -1  0)]
        [(string=? dir "right") (make-posn  1  0)]
        [(string=? dir "up")    (make-posn  0 -1)]
        [(string=? dir "down")  (make-posn  0  1)]))
 

;; -----------------------------------------------------------------------------
;; Loc functions

;; loc+ : Loc Loc -> Loc
;; Bounded addition of locations
(check-expect (loc+ (make-posn 0 1) (make-posn 1 2)) (make-posn 1 3))
(check-expect (loc+ (make-posn (sub1 WIDTH) (sub1 HEIGHT)) (make-posn 1 2))
              (make-posn (sub1 WIDTH) (sub1 HEIGHT)))
(define (loc+ l1 l2)
  (make-posn (min (sub1 WIDTH) (max 0 (+ (posn-x l1) (posn-x l2))))
             (min (sub1 HEIGHT) (max 0 (+ (posn-y l1) (posn-y l2))))))

;; loc-scale : Loc Rational -> Loc
;; Scale a location by a given factor
(check-expect (loc-scale (make-posn 1 2) 3/4) (make-posn 3/4 3/2))
(define (loc-scale l r)
  (make-posn (* (posn-x l) r)
             (* (posn-y l) r)))
  

;; loc-grid-aligned? : Loc -> Boolean
;; Is the given location grid aligned?
(check-expect (loc-grid-aligned? (make-posn 0 0)) #true)
(check-expect (loc-grid-aligned? (make-posn 0 3/2)) #false)
(define (loc-grid-aligned? loc)
  (and (integer? (posn-x loc))
       (integer? (posn-y loc))))

;; loc=? : Loc Loc -> Boolean
;; Are the two given locations the same?
(check-expect (loc=? (make-posn 3 4) (make-posn 3 4)) #true)
(check-expect (loc=? (make-posn 3 4) (make-posn 4 3)) #false)
(define (loc=? l1 l2)
  (and (= (posn-x l1) (posn-x l2))
       (= (posn-y l1) (posn-y l2))))


;; -----------------------------------------------------------------------------
;; Drawing functions

;; game-draw : Game -> Scene
;; Render the game as a scene
(check-expect (game-draw GAME0) (game-draw-on GAME0 SCENE))
(define (game-draw g)
  (game-draw-on g SCENE))

;; score-draw : Number -> Scene
;; Render final score as a scene
(check-expect (score-draw 5)
              (overlay (text "5" GRID-SIZE "red") SCENE))
(define (score-draw s)
  (overlay (text (number->string s) GRID-SIZE "red") SCENE))

;; game-draw-on : Game Scene -> Scene
;; Draw given game on given scene
(check-expect (game-draw-on GAME0 SCENE)
              (pac-draw-on PAC0 (dots-draw-on DOTS0 SCENE)))
(define (game-draw-on g scn)
  (pac-draw-on (game-pac g)
               (dots-draw-on (game-dots g)                             
                             scn)))

;; pac-draw-on : PacMan Scene -> Scene
;; Draw give pac on given scene
(check-expect (pac-draw-on PAC0 SCENE)
              (pac-draw-loc-on (pac-loc PAC0) SCENE))
(define (pac-draw-on p scn)
  (pac-draw-loc-on (pac-loc p) scn))

;; pac-draw-loc-on : Loc Scene -> Scene
;; Draw pacman on scene at given location
(check-expect (pac-draw-loc-on (make-posn 0 0) SCENE)
              (img-draw-on-px PAC-IMG (loc->px (make-posn 0 0)) SCENE))
(define (pac-draw-loc-on loc scn)
  (img-draw-on-px PAC-IMG (loc->px loc) scn))

;; dots-draw-on : Dots Scene -> Scene
;; Draw dots on scene
(check-expect (dots-draw-on '() SCENE) SCENE)
(check-expect (dots-draw-on (list (make-posn 0 0)) SCENE)
              (dot-draw-loc-on (make-posn 0 0) SCENE))
(define (dots-draw-on ps scn)
  (cond [(empty? ps) scn]
        [(cons? ps)
         (dot-draw-loc-on (first ps)
                          (dots-draw-on (rest ps) scn))]))

;; dot-draw-loc-on : Loc Scene -> Scene
;; Draw a dot at given location on scene
(check-expect (dot-draw-loc-on (make-posn 0 0) SCENE)
              (img-draw-on-px DOT-IMG (loc->px (make-posn 0 0)) SCENE))
(define (dot-draw-loc-on loc scn)
  (img-draw-on-px DOT-IMG (loc->px loc) scn))

;; img-draw-on-px : Image Px Scene -> Scene
;; Draw given image at pixel coordinates on scene
(check-expect (img-draw-on-px PAC-IMG (make-posn 0 0) SCENE)
              (place-image PAC-IMG 0 0 SCENE))
(define (img-draw-on-px img px scn)  
  (place-image img (posn-x px) (posn-y px) scn))

;; loc->px : Loc -> Px
;; Convert a location to pixels coordinates
(check-expect (loc->px (make-posn 0 0)) (make-posn (r->i 0) (r->i 0)))
(check-expect (loc->px (make-posn 3 2)) (make-posn (r->i 3) (r->i 2)))
(define (loc->px loc)
  (make-posn (r->i (posn-x loc))
             (r->i (posn-y loc))))

;; r->i : Rational -> Integer
;; Convert given number to grid centered pixel location
(check-expect (r->i 0) (*  1/2 GRID-SIZE))
(check-expect (r->i 5) (* 11/2 GRID-SIZE))
(define (r->i r)
  (+ (* r GRID-SIZE) (* 1/2 GRID-SIZE)))

