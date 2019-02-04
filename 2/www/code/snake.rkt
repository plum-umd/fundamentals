#lang class/0

;; Run (main GAME0) to play.

;; main : Game -> Game
;; Run the snake game using `big-bang' given the initial world state.
#;
(define (main g)
  (big-bang g                          ; (Game)
            [to-draw   draw-game]      ; (Game -> Image)
            [on-tick   tock-game 1/4]  ; (Game -> Game)
            [on-key    keyh-game]      ; (Game KeyEvent -> Game)
            [stop-when stop-game]))    ; (Game -> Boolean)

(require 2htdp/image)
;(require 2htdp/universe)
(require class/universe)

;; A Game is a (new game% Snake)
(define-class game%
  (fields snake #;food)

  ;; to-draw : -> Image
  (define (to-draw)
    (send (send this snake) draw-on MT))

  ;; on-tick : -> Game
  (define (on-tick) this)

  ;; on-key : KeyEvent -> Game
  (define (on-key ke) this)

  ;; stop-when : -> Boolean
  (define (stop-when) #false))

;; A Snake is a (new snake% Seg [Listof Seg] Dir)
(define-class snake%
  (fields head tail dir)

  ;; draw-on : Scene -> Scene
  ;; Draw this snake on the given scene
  (define (draw-on scn)
    (send (send this head) draw-on
          (send (send this tail) draw-on scn))))

;; A Food is a ...
(define-class food%
  (fields ))

;; A Seg is a (new seg% Int Int)
(define-class seg%
  (fields x y)

  ;; draw-on : Scene -> Scene
  ;; Draws this segment on the given scene
  (define (draw-on scn)
     (place-image (square BLOCK-SCALE "solid" SEGCOLOR)
               (* (+ 1 (send this x)) BLOCK-SCALE)
               (* (+ 1 (send this y)) BLOCK-SCALE)
               scn)))
               

;; A Dir is one of:
;; - (new up%)
;; - (new down%)
;; - (new left%)
;; - (new right%)

(define-class up%)
(define-class down%)
(define-class left%)
(define-class right%)

;; A ListofSeg is one of:
;; - (new empty%)
;; - (new cons% Seg ListofSeg)

(define-class empty%

  ;; draw-on : Scene -> Scene
  ;; Draw this empty list of segments on the given scene
  (define (draw-on scn) scn))
  
(define-class cons%
  (fields first rest)

  ;; draw-on : Scene -> Scene
  ;; Draw this non-empty list of segments on the given scene
  (define (draw-on scn)
    (send (send this first) draw-on
          (send (send this rest) draw-on scn))))


     


  

;; A Food is a (make-posn Int Int)
;; Interp: the location of a block of food in the game.

;; food-template : Food -> ???
(define (food-template f)
  (... (posn-x f) ... (posn-y f) ...))

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

;; A Dir is one of: "left", "right", "up", or "down".
;; Interp: the direction in which the head of the snake is moving.

;; dir-template : Dir -> ???
(define (dir-template d)
  (cond [(string=? "left"  d) ...]
        [(string=? "right" d) ...]
        [(string=? "up"    d) ...]
        [(string=? "down"  d) ...]))

;; A Seg is (make-posn Int Int)
;; Interp: the location of one piece of the snake

;; seg-template : Seg -> ???
(define (seg-template s)
  (... (posn-x s) ... (posn-y s) ...))

;; A NEListofSeg is one of:
;; - (cons Seg '())
;; - (cons Seg NEListofSeg)
;; Interp: the segments of the snake in the game, where the first is the snake's
;; head.


;; segs-template : NEListofSeg -> ???
(define (segs-template ss)
  (cond [(empty? (rest ss)) (... (first ss) ...)]
        [(cons?  (rest ss)) (... (first ss)
                                 ...
                                 (segs-template (rest ss)))]))


;; A Snake is a (make-snake NEListofSeg Dir)
;; Interp: All of the segments inside the snake and the direction
;; that the snake is moving.
(define-struct snake (segs dir))

;; snake-template : Snake -> ???
(define (snake-template s)
  (... (segs-template (snake-segs s)) ... 
       (dir-template (snake-dir s)) ...))

;; A Game is a (make-game Snake Food)
;; Interp: the WorldState of the snake game.
(define-struct game (snake food))

;; game-template : Game -> ???
(define (game-template g)
  (... (snake-template (game-snake g)) ...
       (food-template (game-food g)) ...))

;; A Block is a (make-posn Int Int)
;; Interp: the smallest unit of measure on the game board.

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

(define GAME0 (make-game SNAKE0 FOOD0))
;; eating on next turn:
(define GAME1 (make-game SNAKE1 FOOD0))
(define GAME2 (make-game SNAKE2 FOOD2))
;; collision with self:
(define GAMEOVER0 (make-game (make-snake (list* (make-posn 0 0)
                                                (make-posn 1 0)
                                                SEGS2)
                                         "left")
                             FOOD2))
;; collision with wall:
(define GAMEOVER1 (make-game (make-snake (cons (make-posn -1 0) SEGS0) "left")
                             FOOD2))


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
               (* (+ 1 (posn-y b)) BLOCK-SCALE)
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
                  (draw-block-on (game-food g) FOODCOLOR MT)))


;; eating? : Snake Food -> Boolean
;; Is the snake eating food in the next state?
(define (eating? s fs)
  (seg-food=? (next-head s) fs))

(check-expect (eating? SNAKE0 FOOD0) #false)
(check-expect (eating? SNAKE1 FOOD0) #true)
(check-expect (eating? SNAKE2 FOOD2) #false)

;; seg-food=? : Seg Food -> Boolean
;; Is the given segment at the same coordinate as given food
(define (seg-food=? s f)
  (and (= (posn-x s) (posn-x f))
       (= (posn-y s) (posn-y f))))

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

(check-expect (next-head SNAKE0) (make-posn (posn-x (current-head SNAKE0))
                                            (+ 1 (posn-y (current-head SNAKE0)))))
(check-expect (next-head SNAKE1) (make-posn (+ 1 (posn-x (current-head SNAKE1)))
                                            (posn-y (current-head SNAKE1))))
(check-expect (next-head SNAKE2) (make-posn (posn-x (current-head SNAKE2))
                                            (- (posn-y (current-head SNAKE2)) 1)))

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


;; maybe-add-food : Any -> Food
;; Create food somewhere on board.
(define (random-food _)
  (make-posn (random MTW) (random MTH)))

(check-random (random-food '_)
              (make-posn (random MTW) (random MTH)))

;; tock-game : Game -> Game
;; Change the state of the snake game after one clock tick.
(define (tock-game g)
  (cond [(eating? (game-snake g) (game-food g)) (eat&grow g)]
        [else (make-game (move-snake (game-snake g))
                         (game-food g))]))

;; eat&grow : Game -> Game
;; Eat the current food, grow the snake, and generate new food.
(define (eat&grow g)
  (make-game (grow-snake (game-snake g))
             (random-food '_)))


(check-expect (tock-game GAME0) ; not eating
              (make-game (move-snake SNAKE0)
                         (game-food GAME0)))
(check-random (tock-game GAME1) ; eating
              (make-game (grow-snake SNAKE1)
                         (random-food '_)))
(check-random (tock-game GAME2) ; not eating
              (make-game (move-snake SNAKE2)
                         (game-food GAME2)))


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


;; keyh-game : Game KeyEvent -> Game
;; Handle big-bang key events for the snake game.
(define (keyh-game g ke)
  (cond [(or (string=? ke "left") (string=? ke "right")
             (string=? ke "down") (string=? ke "up"))
         (make-game (make-snake (snake-segs (game-snake g)) ke)
                    (game-food g))]
        [else g]))

(check-expect (keyh-game GAME0 "right")
              (make-game (make-snake SEGS0 "right") FOOD0))
(check-expect (keyh-game GAME1 "left")
              (make-game (make-snake SEGS1 "left") FOOD0))
(check-expect (keyh-game GAME2 "down")
              (make-game (make-snake SEGS2 "down") FOOD2))


