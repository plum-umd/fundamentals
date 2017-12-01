;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname snake-in-progress) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; A Game is a (make-game Snake ListofFood)
(define-struct game (snake foods))

;; game-template : Game -> ???
;; Given any game, we can tear it apart.
(define (game-template g)
  (... (snake-template (game-snake g)) ...
       (foods-template (game-foods g)) ...))

;; A ListofFood is one of:
;; - '()
;; - (cons Food ListofFood)

;; foods-template : ListofFood -> ???
;; A Food is one of two things, possibly recursive.
(define (foods-template fs)
  (cond [(empty? fs) ...]
        [(cons? fs)
         (... (first fs)
              ...
              (foods-template (rest fs)))]))

;; A Food is a (make-posn Int Int)
;; Interp: The location of a block of food in the game.

;; food-template : Food -> ???
(define (food-template f)
  (... (posn-x f) ... (posn-y f) ...))

;; A Snake is a (make-snake NEListofSeg Dir)
;; Interp: All of the segments inside the snake, and the direction
;; that the snake is moving.
(define-struct snake (segs dir))

;; snake-template : Snake -> ???
(define (snake-template s)
  (... (segs-template (snake-segs s)) ... 
       (dir-template (snake-dir s)) ...))

;; A NEListofSeg is one of:
;; - (cons Seg '())
;; - (cons Seg NEListofSeg)
;; Interp: the body of the snake in the game

;; segs-template : NEListofSeg -> ???
(define (segs-template ss)
  (cond [(empty? (rest ss)) (... (first ss) ...)]
        [(cons? (rest ss))
         (... (first ss)
              ...
              (segs-template (rest ss)))]))

;; A Seg is (make-posn Int Int)
;; Interp: the location of one piece of the snake

;; A Dir is one of:
;; - "left"
;; - "right"
;; - "up"
;; - "down"
;; Interp: the direction in which the head of the snake is going.
(define (dir-template d)
  (cond [(string=? "left" d) ...]
        [(string=? "right" d) ...]
        [(string=? "up" d) ...]
        [(string=? "down" d) ...]))

;; ------------- Example data:

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
(define GAME1 (make-game SNAKE1 FOODS1))
(define GAME2 (make-game SNAKE2 FOODS2))
;; collides with self on next tick:
(define GAMEOVER0 (make-game (make-snake (list* (make-posn 0 0)
                                                (make-posn 1 0)
                                                SEGS2)
                                         "left")
                             FOODS3))
;; collides with wall:
(define GAMEOVER1 (make-game (make-snake (cons (make-posn -1 0) SEGS0) "left")
                             FOODS2))


;; ---------- Constants

;; Used in drawing functions
(define SCALE 10)
(define MTW 40)
(define MTH 30)
(define MT (rectangle (* (+ 1 MTW) SCALE)
                      (* (+ 1 MTH) SCALE)
                      "solid"
                      "black"))

;; Used in `game-over'
(define GAMEOVER-SIZE 32)
(define GAMEOVER-COLOR "red")
(define GAMEOVER-MSG "Final score: ")


;; --------- Main function (and our wish-list)

;; main : Game -> Game
;; Run the snake game using `big-bang' given the initial world state.
(define (main g)
  (big-bang g #;(Game)
            [on-tick   tock-game 1/4 #;(Game -> Game)]
            [stop-when stop-game #;(Game -> Boolean)
                       game-over #;(Game -> Image)]
            [to-draw   draw-game #;(Game -> Image)]   
            [on-key    keyh-game #;(Game KeyEvent -> Game)]))


;; ---------------- Drawing functions

;; A Block is a (make-posn x y),
;; where X and Y are integer values representing unscaled
;; positions on our game board.

;; draw-block-on : Block Color Image -> Image
;; Places a square of color C at the scaled pixel
;; location denoted by B on SCN.
(define (draw-block-on b c scn)
  (place-image (square SCALE "solid" c)
               (* (+ 1 (posn-x b)) SCALE)
               (* (+ 1 (posn-y b)) SCALE)
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



;; --------------- Ticking functions

;; current-head : Snake -> Seg
;; Return the head segment of the given snake.
(define (current-head s)
  (first (snake-segs s)))

(check-expect (current-head SNAKE0) SEG0)
(check-expect (current-head SNAKE1) SEG1)
(check-expect (current-head SNAKE2) SEG2)

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

(check-expect (next-head SNAKE0)
              (make-posn (posn-x (current-head SNAKE0))
                         (+ 1 (posn-y (current-head SNAKE0)))))
(check-expect (next-head SNAKE1)
              (make-posn (+ 1 (posn-x (current-head SNAKE1)))
                         (posn-y (current-head SNAKE1))))
(check-expect (next-head SNAKE2)
              (make-posn (posn-x (current-head SNAKE2))
                         (- (posn-y (current-head SNAKE2)) 1)))

;; eating? : Game -> Boolean
;; Is the snake eating food in the next state?
(define (eating? g)
  (member? (next-head (game-snake g))
           (game-foods g)))

(check-expect (eating? GAME0) #false)
(check-expect (eating? GAME1) #true)
(check-expect (eating? GAME2) #false)

;; remove-tail : NELoSeg -> LoSeg
;; Removes the last element from the given NELoSeg.
;; Note: the returned list may not be non-empty.
(define (remove-tail ss)
  (cond [(empty? (rest ss)) '()]
        [else (cons (first ss) (remove-tail (rest ss)))]))

(check-expect (remove-tail SEGS0) '())
(check-expect (remove-tail SEGS1)
              (list (make-posn 0 1)))

;; return-last : NELoSeg -> Seg
;; Returns the last element.
;; Note: almost identical to remove-tail.
(define (return-last ss)
  (cond [(empty? (rest ss)) (first ss)]
        [else (return-last (rest ss))]))

(check-expect (return-last SEGS0) SEG0)
(check-expect (return-last SEGS1) SEG0)
(check-expect (return-last (reverse SEGS2)) SEG2)

;; move-snake : Snake -> Snake
;; Move the snake one segment in its current direction.
(define (move-snake s)
  (make-snake (cons (next-head s) (remove-tail (snake-segs s)))
              (snake-dir s)))

(check-expect (move-snake SNAKE0) (make-snake (list (make-posn 0 1)) "down"))
(check-expect (move-snake SNAKE1) (make-snake (list (make-posn 1 1)
                                                    (make-posn 0 1))
                                              "right"))

;; grow-snake : Snake -> Snake
;; Grow the snake one segment in its current direction.
(define (grow-snake s)
  (make-snake (cons (next-head s) (snake-segs s))
              (snake-dir s)))

(check-expect (grow-snake SNAKE0) (make-snake (list (make-posn 0 1)
                                                    (make-posn 0 0))
                                              "down"))
(check-expect (grow-snake SNAKE1) (make-snake (list (make-posn 1 1)
                                                    (make-posn 0 1)
                                                    (make-posn 0 0))
                                              "right"))

;; maybe-add-food : Game -> Game
;; Add food under the tail of the snake about FOOD-FREQ⁻¹ often.
(define FOOD-FREQ 5)
(define (maybe-add-food g)
  (cond [(= 0 (random FOOD-FREQ))
         (make-game (game-snake g)
                    (cons (return-last (snake-segs (game-snake g)))
                          (game-foods g)))]
        [else g]))

(check-random (maybe-add-food GAME0)
              (cond [(= 0 (random FOOD-FREQ))
                     (make-game SNAKE0 (cons (return-last SEGS0) FOODS0))]
                    [else GAME0]))
(check-random (maybe-add-food GAME1)
              (cond [(= 0 (random FOOD-FREQ))
                     (make-game SNAKE1 (cons (return-last SEGS1) FOODS1))]
                    [else GAME1]))
(check-random (maybe-add-food GAME2)
              (cond [(= 0 (random FOOD-FREQ))
                     (make-game SNAKE2 (cons (return-last SEGS2) FOODS2))]
                    [else GAME2]))

;; tock-game : Game -> Game
;; Change the state of the snake game after one clock tick.
;; What must happen?
;; - snake must move if we're not eating,
;; - snake should grow if we are,
;; - food must appear randomly (~ 20%)
(define (tock-game g)
  (maybe-add-food
   (cond [(eating? g)
          (make-game (grow-snake (game-snake g))
                     (remove (next-head (game-snake g))
                             (game-foods g)))]
         [else (make-game (move-snake (game-snake g))
                          (game-foods g))])))

(check-random (tock-game GAME0) ; not eating
              (maybe-add-food
               (make-game (move-snake SNAKE0) FOODS0)))
(check-random (tock-game GAME1) ; eating
              (maybe-add-food
               (make-game (grow-snake SNAKE1)
                          (remove (next-head SNAKE1) FOODS1))))
(check-random (tock-game GAME2) ; not eating
              (maybe-add-food
               (make-game (move-snake SNAKE2) FOODS2)))

;; --------------- Stop the world!

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

;; on-board? : Seg -> Boolean
;; Returns true only if the given segment is on the game board.
(define (on-board? s)
  #;(... (posn-x s) ... (posn-y s) ...)
  #false)
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
  #;(... (game-snake g) ... (game-foods g) ...)
  g)

(check-expect (stop-game GAME0) #false)
(check-expect (stop-game GAME1) #false)
(check-expect (stop-game GAME2) #false)
(check-expect (stop-game GAMEOVER0) #true)
(check-expect (stop-game GAMEOVER1) #true)

;; --------------- Key-handling functions

;; keyh-game : Game KeyEvent -> Game
;; Handle big-bang key events for the snake game.
(define (keyh-game g ke)
  #;(... (game-snake g) ... (game-foods g) ...)
  g)

(check-expect (keyh-game GAME0 "right")
              (make-game (make-snake SEGS0 "right") FOODS0))
(check-expect (keyh-game GAME1 "left")
              (make-game (make-snake SEGS1 "left") FOODS1))
(check-expect (keyh-game GAME2 "down")
              (make-game (make-snake SEGS2 "down") FOODS2))
